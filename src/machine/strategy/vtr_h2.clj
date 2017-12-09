(ns machine.strategy.vtr-h2
  (:require [clojure.core.async :refer [chan close! go go-loop >! <! alts! thread timeout pub sub put!]]
            [mount.core :refer [defstate]]
            [schema.core :as s]
            [clj-time.core :as dt]
            [taoensso.timbre :as timbre]
            [taoensso.encore :refer [reset-in! round when-lets]]
            [machine.utils :refer :all]
            [machine.settings.core :refer [settings]]
            [machine.router.core :as router]
            [machine.db.core :as db :refer [DB]]
            [machine.quotes.core :as q]
            [machine.quotes.series :refer :all]
            [machine.time-management.core :as tim]
            [machine.time-management.market-hours :as market-hours]
            [machine.data-management.core :as dm]
            [machine.analyse-management.wrb-analysis.utils :refer :all]
            [machine.oanda.core :as oanda]
            [machine.strategy.schema :refer :all]
            [machine.strategy.core :as strategy]
            [machine.money-management.core :as mm]
            [machine.money-management.schema :refer :all]
            [machine.trade-management.core :refer :all]
            [machine.trade-management.permission :as permission]
            [machine.analyse-management.wrb-analysis.vtr :as vtr])
  (:import [org.apache.commons.math3.util FastMath]
           [machine.oanda.core OandaAccount]
           [machine.strategy.core Strategy]))

(declare VTR-H2 VTR-H2-EA)

(s/defn look-for-vtr-h2-trade
  ([{:keys [dcm? markets]}
    {:keys [^objects time# ^longs vtr-h2# ^longs wrb-dcm-trend# instrument period] :as data} i]
    (if (aget time# i)
      (when (and vtr-h2# (or (not dcm?) wrb-dcm-trend#)
                 (or (not (seq markets))
                     (some (set markets) (market-hours/active-markets (aget time# i)))))
        (cond
          (and (== 1 (aget vtr-h2# i))                         ;(> (double (aget volume# i)) (aget volume-average# i))
               (or (not dcm?) (pos? (aget wrb-dcm-trend# i)))) :buy
          (and (== -1 (aget vtr-h2# i))                        ;(> (double (aget volume# i)) (aget volume-average# i))
               (or (not dcm?) (neg? (aget wrb-dcm-trend# i)))) :sell
          :else nil))
      (timbre/error "[time-error] -" [instrument period i])))
  ([strategy data]
    (look-for-vtr-h2-trade strategy data 0)))


(s/defn vtr-h2-stop-loss-fn :- s/Num
  [strategy side {:keys [^doubles open-bid# ^doubles open-ask#] :as data} i]
  (case side
    :buy (aget open-bid# i)
    :sell (aget open-ask# i)))


(s/defn vtr-h2-strategy :- Strategy
  [account :- OandaAccount]
  (merge (strategy/strategy :vtr account
                            {:open-trade-fn look-for-vtr-h2-trade
                             :stop-loss-fn  vtr-h2-stop-loss-fn})
         (:money account)
         (get-in account [:strategy :vtr-h2])))


(s/defn start-analyse-worker
  [{{:keys [account-id]} :account :keys [zones instruments periods] :as strategy}]
  (let [wait-time (-> (sort-by tim/period->seconds periods)
                      (first)
                      (tim/period->seconds)
                      (* 1.1 1000))
        correlation-analyse-event (chan 1 (comp (filter #((set instruments) (:instrument %))))
                                        (comp (filter #((set periods) (:period %)))))]
    (sub router/publication :analyse/correlation correlation-analyse-event
         (go-loop []
           (when-let [[v p] (alts! [correlation-analyse-event (timeout wait-time)])]
             (if (= p correlation-analyse-event)
               (let [{main-instrument :instrument main-data :data
                      :keys           [period time]} v]
                 (timbre/info "[vtr-h2-analyse-worker] -" [account-id main-instrument period]
                              "- going to analyse instruments")
                 (if (market-hours/within-trading-hours? main-instrument (dt/now))
                   (let [analysed (->> dm/instruments
                                       (remove #(= main-instrument %))
                                       (map #(db/get-quotes % period))
                                       (reduce (fn [d1 d2]
                                                 (vtr/volatility-trading-report d1 d2 zones))
                                               main-data))
                         [last-vtr vtr-dir] (->> (map vector (:time# analysed) (:vtr-h2# analysed))
                                                 (filter (fn [[t v]]
                                                           (not (zero? v))))
                                                 (first))]
                     (timbre/info "[vtr-h2-analyse-worker] - debug -" [main-instrument period]
                                  "- last vtr -" [vtr-dir last-vtr])
                     (timbre/info "[vtr-h2-analyse-worker] -" [main-instrument period]
                                  "- analysed quotes successful")
                     (db/save-quotes! main-instrument period analysed)
                     (when-let [[_ p] (alts! [[router/channel {:event      :analyse/vtr
                                                               :instrument main-instrument
                                                               :period     period
                                                               :data       analysed
                                                               :time       time}]
                                              (timeout 10000)])]
                       (when-not (= p router/channel)
                         (timbre/error "[vtr-h2-analyse-worker] -" [main-instrument period]
                                       "can't put to the channel"))))
                   (do (timbre/error "[vtr-h2-analyse-worker] - outside trading hours!")
                       (<! (timeout 60000)))))
               (timbre/error "[vtr-h2-analyse-worker] - can't take data from the channel")))
           (recur)))))


(s/defn start-executor-worker
  [{{:keys [account-id] :as account} :account
    :keys                            [max-risk max-dd min-margin-level max-spread-percent max-open-trades
                                      periods markets]
    :as                              strategy}]
  (let [wait-time (-> (sort-by tim/period->seconds periods)
                      (first)
                      (tim/period->seconds)
                      (* 1.1 1000))
        analyse-channel (chan)]
    (timbre/info "[vtr-h2-executor-worker] -" [account-id] "- starting!")
    (sub router/publication :analyse/vtr analyse-channel)
    (go-loop []
      (when-let [[v p] (alts! [analyse-channel (timeout wait-time)])]
        (if (= p analyse-channel)
          (let [{:keys [instrument period data time]} v]
            (timbre/info "[vtr-h2-executor-worker] -" [account-id instrument period]
                         "look for open trade")
            (if (market-hours/within-trading-hours? instrument (dt/now))
              (if (or (not (seq markets))
                      (some (set markets) (market-hours/active-markets time)))
                (when-let [{:keys [side open-time open-price stop-loss-price take-profit-price]
                            :as   tmp-trade} (strategy/prepare-trade strategy data 0)]
                  (timbre/info "[vtr-h2-executor-worker] -" [account-id instrument period side]
                               "- found trade!")
                  (let [trade-size (mm/redd-trade-size (assoc tmp-trade
                                                         :account account
                                                         :max-risk max-risk
                                                         :max-dd max-dd
                                                         :balance (oanda/balance account)))
                        stop-loss-pips (FastMath/abs ^double (- open-price stop-loss-price))]
                    (when (and (permission/instrument-active? account instrument)
                               (permission/positive-trade-size? trade-size)
                               (or (permission/time-slippage-alloved? open-time (dt/seconds 60))
                                   (permission/price-slippage-alloved? account tmp-trade))
                               (permission/spread-allowed? account instrument (* stop-loss-pips
                                                                                 max-spread-percent))
                               (permission/margin-allowed? account min-margin-level)
                               (permission/trades-count-allowed? account max-open-trades))
                      (when-let [{{trade-id :id :as trade-opened}
                                  :trade-opened} (oanda/market-order!
                                                   account
                                                   instrument
                                                   side
                                                   trade-size
                                                   (case side
                                                     :buy (- stop-loss-price (oanda/instrument-spread account instrument))
                                                     :sell (+ stop-loss-price (oanda/instrument-spread account instrument)))
                                                   (case side
                                                     :buy (+ take-profit-price (oanda/instrument-spread account instrument))
                                                     :sell (- take-profit-price (oanda/instrument-spread account instrument))))]
                        (timbre/info "[vtr-h2-executor-worker] -"
                                     [account-id instrument period side strategy]
                                     "- take trade successfull")
                        (when-let [trade (oanda/trade-info account trade-id)]
                          (when-let [[_ p]
                                     (alts! [[router/channel
                                              {:event      :open-trade
                                               :account-id account-id
                                               :instrument instrument
                                               :period     period
                                               :trade      (assoc trade
                                                             :instrument instrument
                                                             :period period
                                                             :strategy (strategy/name strategy)
                                                             :entry-stop-loss-price stop-loss-price)}]
                                             (timeout 10000)])]
                            (when-not (= p router/channel)
                              (timbre/error "[vtr-h2-executor-worker] -"
                                            [account-id instrument period side strategy]
                                            "can't put to the channel"))))))))
                (timbre/error "[vtr-h2-executor-worker] -" [account-id]
                              "- outside markets hours!" markets))
              (do (timbre/error "[vtr-h2-analyse-worker] - outside trading hours!")
                  (<! (timeout 60000)))))
          (timbre/error "[vtr-h2-executor-worker] -" [account-id]
                        "- can't take data from the channel")))
      (recur))))


(s/defn start-watchman-worker
  [{{:keys [account-id] :as account} :account :as strategy}]
  (let [wait-time (-> (sort-by tim/period->seconds dm/periods)
                      (first)
                      (tim/period->seconds)
                      (* 1.1 1000))
        analyse-channel (chan)]
    (timbre/info "[vtr-h2-watchman-worker] - starting worker!")
    (sub router/publication :analyse/vtr analyse-channel)
    (go-loop []
      (when-let [[v p] (alts! [analyse-channel (timeout wait-time)])]
        (if (= p analyse-channel)
          (let [{:keys [instrument period data]} v
                trades (->> (get-in @DB [:open-trades account-id])
                            (filter (fn [{trade-instrument :instrument
                                          trade-period     :period
                                          trade-strategy   :strategy}]
                                      (= [trade-instrument trade-period trade-strategy]
                                         [instrument period (strategy/name strategy)])))
                            (remove nil?))]
            (if (market-hours/within-trading-hours? instrument (dt/now))
              (doseq [{:keys [id stop-loss take-profit instrument period] :as trade} trades]
                (timbre/debug "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                              "- found trade")
                (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                             "- look for move stop-loss")
                (timbre/info "[vtr-h2-watchman-worker] - check trailing"
                             [account-id instrument period id]
                             (strategy/trailing-stop
                               strategy
                               (clojure.set/rename-keys
                                 trade
                                 {:price       :open-price
                                  :stop-loss   :stop-loss-price
                                  :take-profit :take-profit-price})
                               data))
                (timbre/info "[vtr-h2-watchman-worker] - check be"
                             [account-id instrument period id]
                             (strategy/break-even
                               strategy
                               (clojure.set/rename-keys
                                 trade
                                 {:price       :open-price
                                  :stop-loss   :stop-loss-price
                                  :take-profit :take-profit-price})
                               data))
                (when-let [new-stop-loss (or (strategy/trailing-stop
                                               strategy
                                               (clojure.set/rename-keys
                                                 trade
                                                 {:price       :open-price
                                                  :stop-loss   :stop-loss-price
                                                  :take-profit :take-profit-price})
                                               data)
                                             (strategy/break-even
                                               strategy
                                               (clojure.set/rename-keys
                                                 trade
                                                 {:price       :open-price
                                                  :stop-loss   :stop-loss-price
                                                  :take-profit :take-profit-price})
                                               data))]
                  (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                               "- look for move stop-loss")
                  (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                               "- old-stop-loss:" stop-loss
                               "new-stop-loss:" new-stop-loss)
                  (when-let [{new-stop-loss :stop-loss :as modified-trade}
                             (oanda/modify-trade! account id new-stop-loss take-profit)]
                    (update-trade-in-db! (assoc trade :take-profit new-stop-loss))
                    (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                                 "- move stop-loss succesfull")
                    (when-let [[_ p] (alts! [[router/channel {:event      :modify-trade
                                                              :instrument instrument
                                                              :period     period
                                                              :trade      modified-trade}]
                                             (timeout 10000)])]
                      (when-not (= p router/channel)
                        (timbre/error "[vtr-h2-watchman-worker] -" [account-id]
                                      "- can't put to the channel")))))
                (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                             "- look for move take-profit")
                (when-let [new-take-profit (strategy/trailing-profit
                                             strategy
                                             (clojure.set/rename-keys
                                               trade
                                               {:price       :open-price
                                                :stop-loss   :stop-loss-price
                                                :take-profit :take-profit-price})
                                             data)]
                  (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                               "- old-take-profit:" take-profit
                               "new-take-profit:" new-take-profit)
                  (when-let [{new-take-profit :take-profit :as modified-trade}
                             (oanda/modify-trade! account id stop-loss new-take-profit)]
                    (update-trade-in-db! (assoc trade :take-profit new-take-profit))
                    (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                                 "- move take-profit succesfull")
                    (when-let [[_ p] (alts! [[router/channel {:event      :modify-trade
                                                              :instrument instrument
                                                              :period     period
                                                              :trade      modified-trade}]
                                             (timeout 10000)])]
                      (when-not (= p router/channel)
                        (timbre/error "[vtr-h2-watchman-worker] -" [account-id]
                                      "- can't put to the channel")))))
                (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                             "- look for close trade")
                (when (strategy/look-for-close-trade strategy trade data)
                  (when-let [{:keys [profit] :as closed-trade} (oanda/close-trade! account id)]
                    (timbre/info "[vtr-h2-watchman-worker] -" [account-id instrument period id]
                                 "- close trade succesfull with profit" profit)
                    (when-let [[_ p] (alts! [[router/channel {:event      :close-trade
                                                              :instrument instrument
                                                              :period     period
                                                              :trade      closed-trade}]
                                             (timeout 10000)])]
                      (when-not (= p router/channel)
                        (timbre/error "[vtr-h2-watchman-worker] -" [account-id]
                                      "- can't put to the channel"))))))
              (do (timbre/error "[vtr-h2-analyse-worker] - outside trading hours!")
                  (<! (timeout 60000)))))
          (timbre/error "[vtr-h2-watchman-worker] -" [account-id]
                        "- can't take from the channel")))
      (recur))))


(defstate VTR-H2
          :start (->> oanda/oanda-accounts
                      (filter (fn [account] (get-in account [:strategy :vtr-h2])))
                      (mapv (fn [account] (vtr-h2-strategy account))))
          :stop [])

(defstate VTR-H2-EA
          :start (mapv (fn [strategy]
                         (assoc strategy
                           :analyse (start-analyse-worker strategy)
                           :executor (start-executor-worker strategy)
                           :watchman (start-watchman-worker strategy)))
                       VTR-H2)
          :stop (mapv (fn [strategy]
                        (close! (:analyse strategy))
                        (close! (:executor strategy))
                        (close! (:watchman strategy)))
                      VTR-H2-EA))