(ns machine.strategy.vsa
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


(declare VSA VSA-EA)

(s/defn look-for-vsa-trade
  ([{:keys [dcm? markets]}
    {:keys [^objects time# ^longs vsa-sd# ^longs wrb-dcm-trend# instrument period] :as data} i]
    (if (aget time# i)
      (when (and vsa-sd# (or (not dcm?) wrb-dcm-trend#)
                 (or (not (seq markets))
                     (some (set markets) (market-hours/active-markets (aget time# i)))))
        (cond
          (and (== 1 (aget vsa-sd# i))                         ;(> (double (aget volume# i)) (aget volume-average# i))
               (or (not dcm?) (pos? (aget wrb-dcm-trend# i)))) :buy
          (and (== -1 (aget vsa-sd# i))                        ;(> (double (aget volume# i)) (aget volume-average# i))
               (or (not dcm?) (neg? (aget wrb-dcm-trend# i)))) :sell
          :else nil))
      (timbre/error "[time-error] -" [instrument period i])))
  ([strategy data]
    (look-for-vsa-trade strategy data 0)))


(s/defn vsa-stop-loss-fn :- s/Num
  [strategy side
   {:keys [^doubles low-bid# ^doubles high-ask#] :as data} i]
  (case side
    :buy (FastMath/min ^double (aget low-bid# i) ^double (aget low-bid# (inc i)))
    :sell (FastMath/max ^double (aget high-ask# i) ^double (aget high-ask# (inc i)))))


(s/defn vsa-strategy :- Strategy
  [account :- OandaAccount]
  (merge (strategy/strategy :vsa account
                            {:open-trade-fn look-for-vsa-trade
                             :stop-loss-fn  vsa-stop-loss-fn})
         (:money account)
         (get-in account [:strategy :vsa])))

(defstate VSA
          :start (->> oanda/oanda-accounts
                      (filter (fn [account] (get-in account [:strategy :vsa])))
                      (mapv (fn [account] (vsa-strategy account))))
          :stop [])

;(defstate VSA-EA
;          :start (mapv (fn [strategy]
;                         (assoc strategy
;                           :analyse (start-analyse-worker strategy)
;                           :executor (start-executor-worker strategy)
;                           :watchman (start-watchman-worker strategy)))
;                       VSA)
;          :stop (mapv (fn [strategy]
;                        (close! (:analyse strategy))
;                        (close! (:executor strategy))
;                        (close! (:watchman strategy)))
;                      VSA-EA))