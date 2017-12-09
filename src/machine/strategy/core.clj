(ns machine.strategy.core
  (:refer-clojure :exclude [name])
  (:require [schema.core :as s]
            [taoensso.timbre :as timbre]
            [machine.quotes.series :refer :all]
            [machine.oanda.core :as oanda]
            [machine.utils :refer :all]
            [machine.analyse-management.wrb-analysis.utils :refer :all]
            [machine.time-management.core :as tim]
            [machine.trade-management.permission :as permission])
  (:import [org.apache.commons.math3.util FastMath]
           [org.apache.commons.lang3 ArrayUtils]
           [machine.oanda.core OandaAccount]))


(defrecord Strategy [name account open-trade-fn close-trade-fn
                     stop-loss-fn take-profit-fn
                     trailing-stop-fn trailing-profit-fn
                     break-even-fn])

(defprotocol PStrategy
  (name [strategy])
  (look-for-open-trade [strategy data] [strategy data i])
  (look-for-close-trade [strategy trade data] [strategy trade data i])
  (stop-loss [strategy side data] [strategy side data i])
  (take-profit [strategy side data] [strategy side data i])
  (trailing-stop [strategy trade data] [strategy trade data i])
  (trailing-profit [strategy trade data] [strategy trade data i])
  (break-even [strategy trade data] [strategy trade data i])
  (prepare-trade [strategy data] [strategy data i])
  (history-trades [strategy data opts]))

(s/defn default-look-for-close-trade-fn :- (s/maybe s/Bool)
  [{:keys [profit-target min-rr]}
   {:keys [side open-time open-price entry-stop-loss-price] :as trade}
   {:keys [^objects time# ^doubles close# ^longs wrb-hg-body#] :as data} i]
  (let [open-candle (ArrayUtils/indexOf time# open-time)]
    (when (and (pos? open-candle) (< i open-candle))
      (case side
        :buy (when (and (identity profit-target) (pos? profit-target)
                        (== 1 (aget wrb-hg-body# (inc i))))
               (let [targets (count (filter (fn [[hg c]] (and (== 1 hg) (> c open-price)))
                                            (map vector
                                                 (aslice wrb-hg-body# i open-candle)
                                                 (aslice close# i open-candle))))]
                 (when (and (> targets profit-target)
                            (>= (aget close# i)
                                (amax (aslice close# i (dec open-candle)))))
                   (let [stop-los-pips (- open-price entry-stop-loss-price)]
                     (> (aget close# i) (+ open-price (* (or min-rr 0) stop-los-pips)))))))
        :sell (when (and (identity profit-target) (pos? profit-target)
                         (== -1 (aget wrb-hg-body# (inc i))))
                (let [targets (count (filter (fn [[hg c]] (and (== -1 hg) (< c open-price)))
                                             (map vector
                                                  (aslice wrb-hg-body# i open-candle)
                                                  (aslice close# i open-candle))))]
                  (when (and (> targets profit-target)
                             (<= (aget close# i)
                                 (amin (aslice close# i (dec open-candle)))))
                    (let [stop-los-pips (- entry-stop-loss-price open-price)]
                      (< (aget close# i) (- open-price (* (or min-rr 0) stop-los-pips)))))))))))


(s/defn default-stop-loss-fn :- s/Num
  [strategy side
   {:keys [^doubles low-bid# ^doubles high-ask#] :as data} i]
  (case side
    :buy (FastMath/min ^double (aget low-bid# i) ^double (aget low-bid# (inc i)))
    :sell (FastMath/max ^double (aget high-ask# i) ^double (aget high-ask# (inc i)))))


(s/defn default-take-profit-fn :- s/Num
  [{:keys [rr] :as strategy} side
   {:keys [^doubles close-bid# ^doubles close-ask# ^doubles spread#] :as data} i]
  (let [open-price (case side
                     :buy (aget close-ask# i)
                     :sell (aget close-bid# i))
        stop-loss-price (stop-loss strategy side data i)
        stop-loss-pips (FastMath/abs ^double (- open-price stop-loss-price))]
    (case side
      :buy (+ open-price (* rr (- stop-loss-pips (aget spread# i))))
      :sell (- open-price (* rr (- stop-loss-pips (aget spread# i))))
      nil)))


(s/defn default-trailing-stop-fn :- (s/maybe s/Num)
  [{{:keys [account-id]} :account :keys [trailing-stop?] :as strategy}
   {:keys [open-price stop-loss-price side instrument period] :as trade}
   {:keys [^doubles open-bid# ^doubles open-ask# ^doubles close-bid# ^doubles close-ask#
           ^longs wrb-hg-body# ^longs wrb-dcm-trend#] :as data} i]
  (case trailing-stop?
    :wrb-hg
    (case side
      :buy (when (== 1 (aget wrb-hg-body# (inc i)))
             (let [trailing-price (aget open-bid# (inc i))]
               (when (and (> trailing-price open-price)
                          (< trailing-price (aget close-ask# i))
                          (> trailing-price stop-loss-price))
                 trailing-price)))
      :sell (when (== -1 (aget wrb-hg-body# (inc i)))
              (let [trailing-price (aget open-ask# (inc i))]
                (when (and (< trailing-price open-price)
                           (> trailing-price (aget close-bid# i))
                           (< trailing-price stop-loss-price))
                  trailing-price))))
    :wrb-hg-dcm
    (case side
      :buy (when (and (== 1 (aget wrb-hg-body# (inc i)))
                      (== 1 (aget wrb-dcm-trend# i)))
             (timbre/info "[vtr-trailing-sl] -" [account-id instrument period] "- found bull wrb-hg")
             (let [trailing-price (aget open-bid# (inc i))]
               (timbre/info "[vtr-trailing-sl] -" [account-id instrument period] "- trailing price"
                            trailing-price)
               (when (and (> trailing-price open-price)
                          (< trailing-price (aget close-ask# i))
                          (> trailing-price stop-loss-price))
                 (timbre/info "[vtr-trailing-sl] -" [account-id instrument period] "- move sl!")
                 trailing-price)))
      :sell (when (and (== -1 (aget wrb-hg-body# (inc i)))
                       (== -1 (aget wrb-dcm-trend# i)))
              (timbre/info "[vtr-trailing-sl] -" [account-id instrument period] "- found bear wrb-hg")
              (let [trailing-price (aget open-ask# (inc i))]
                (timbre/info "[vtr-trailing-sl] -" [account-id instrument period] "- trailing price"
                             trailing-price)
                (when (and (< trailing-price open-price)
                           (> trailing-price (aget close-bid# i))
                           (< trailing-price stop-loss-price))
                  (timbre/info "[vtr-trailing-sl] -" [account-id instrument period] "- move sl!")
                  trailing-price))))
    :strong-continuation
    (case side
      :buy (let [^longs zone# (identity-union (mapv #(get data (zone-keyword %))
                                                    [:zone-sc1 :zone-sc2 :zone-sc3 :zone-sc4]))]
             (when (== 1 (aget zone# (inc i)))
               (let [trailing-price (aget open-bid# (inc i))]
                 (when (and (> trailing-price open-price)
                            (< trailing-price (aget close-ask# i))
                            (> trailing-price stop-loss-price))
                   trailing-price))))
      :sell (let [^longs zone# (identity-union (mapv #(get data (zone-keyword %))
                                                     [:zone-sc1 :zone-sc2 :zone-sc3 :zone-sc4]))]
              (when (== -1 (aget zone# (inc i)))
                (let [trailing-price (aget open-ask# (inc i))]
                  (when (and (< trailing-price open-price)
                             (> trailing-price (aget close-bid# i))
                             (< trailing-price stop-loss-price))
                    trailing-price)))))
    :vtr
    (let [{new-side :side new-stop-loss-price :stop-loss-price} (prepare-trade strategy data i)]
      (when (and (= side new-side))
        (case side
          :buy (when (> new-stop-loss-price stop-loss-price)
                 new-stop-loss-price)
          :sell (when (< new-stop-loss-price stop-loss-price)
                  new-stop-loss-price))))
    nil))


(s/defn default-trailing-profit-fn :- (s/maybe s/Num)
  [{{:keys [account-id]} :account :keys [trailing-profit?] :as strategy}
   {:keys [take-profit-price side instrument period] :as trade}
   {:keys [^doubles open# ^doubles close# ^longs wrb-hg-body#] :as data} i]
  (condp = trailing-profit?
    :wrb-hg
    (case side
      :buy (when (== 1 (aget wrb-hg-body# (inc i)))
             (timbre/info "[vtr-trailing-tp] -" [account-id instrument period] "- found bull wrb-hg")
             (let [open-price (aget close# (inc i))
                   stop-loss-price (aget open# (inc i))
                   stop-loss-pips (FastMath/abs ^double (- open-price stop-loss-price))]
               (timbre/info "[vtr-trailing-tp] -" [account-id instrument period] "- trailing price"
                            (+ take-profit-price (* 2 stop-loss-pips)))
               (timbre/info "[vtr-trailing-tp] -" [account-id instrument period] "- move tp!")
               (+ take-profit-price (* 2 stop-loss-pips))))
      :sell (when (== -1 (aget wrb-hg-body# (inc i)))
              (timbre/info "[vtr-trailing-tp] -" [account-id instrument period] "- found bear wrb-hg")
              (let [open-price (aget close# (inc i))
                    stop-loss-price (aget open# (inc i))
                    stop-loss-pips (FastMath/abs ^double (- open-price stop-loss-price))]
                (timbre/info "[vtr-trailing-tp] -" [account-id instrument period] "- trailing price"
                             (+ take-profit-price (* 2 stop-loss-pips)))
                (timbre/info "[vtr-trailing-tp] -" [account-id instrument period] "- move tp!")
                (- take-profit-price (* 2 stop-loss-pips)))))
    :strong-continuation
    (case side
      :buy (let [^longs zone# (identity-union (mapv #(get data (zone-keyword %))
                                                    [:zone-sc1 :zone-sc2 :zone-sc3 :zone-sc4]))]
             (when (== 1 (aget zone# (inc i)))
               (let [open-price (aget close# (inc i))
                     stop-loss-price (aget open# (inc i))
                     stop-loss-pips (FastMath/abs ^double (- open-price stop-loss-price))]
                 (+ take-profit-price stop-loss-pips))))
      :sell (let [^longs zone# (identity-union (mapv #(get data (zone-keyword %))

                                                     [:zone-sc1 :zone-sc2 :zone-sc3 :zone-sc4]))]
              (when (== -1 (aget zone# (inc i)))
                (let [open-price (aget close# (inc i))
                      stop-loss-price (aget open# (inc i))
                      stop-loss-pips (FastMath/abs ^double (- open-price stop-loss-price))]
                  (- take-profit-price stop-loss-pips)))))
    :vtr
    (let [{new-side :side new-take-profit-price :take-profit-price} (prepare-trade strategy data i)]
      (when (and (= side new-side))
        (case side
          :buy (when (> new-take-profit-price take-profit-price)
                 new-take-profit-price)
          :sell (when (< new-take-profit-price take-profit-price)
                  new-take-profit-price))))
    nil))


(s/defn default-break-even-fn :- (s/maybe s/Num)
  [{:keys [break-even?] :as strategy}
   {:keys [open-price stop-loss-price side] :as trade}
   {:keys [^doubles close-bid# ^doubles close-ask# ^longs wrb-hg-body#] :as data} i]
  (cond
    (and (number? break-even?) (pos? break-even?))
    (case side
      :buy (let [stop-loss-pips (- open-price stop-loss-price)]
             (when (and (> open-price stop-loss-price)
                        (>= (aget close-bid# i) (+ open-price (* break-even? stop-loss-pips))))
               open-price))
      :sell (let [stop-loss-pips (- stop-loss-price open-price)]
              (when (and (< open-price stop-loss-price)
                         (<= (aget close-ask# i) (- open-price (* break-even? stop-loss-pips))))
                open-price)))
    (= :wrb-hg break-even?)
    (case side
      :buy (when (== 1 (aget wrb-hg-body# (inc i)))
             open-price)
      :sell (when (== 1 (aget wrb-hg-body# (inc i)))
              open-price))
    (= :strong-continuation break-even?)
    (case side
      :buy (let [^longs zone# (identity-union (mapv #(get data (zone-keyword %))
                                                    [:zone-sc1 :zone-sc2 :zone-sc3 :zone-sc4]))]
             (when (== 1 (aget zone# (inc i)))
               open-price))
      :sell (let [^longs zone# (identity-union (mapv #(get data (zone-keyword %))
                                                     [:zone-sc1 :zone-sc2 :zone-sc3 :zone-sc4]))]
              (when (== -1 (aget zone# (inc i)))
                open-price)))))


(s/defn strategy :- Strategy
  ([m]
    (map->Strategy m))
  ([name :- s/Keyword account :- OandaAccount
    {:keys [open-trade-fn close-trade-fn
            stop-loss-fn take-profit-fn
            trailing-stop-fn trailing-profit-fn
            break-even-fn]}]
    (map->Strategy
      {:name               name
       :account            account
       :open-trade-fn      open-trade-fn
       :close-trade-fn     (or close-trade-fn default-look-for-close-trade-fn)
       :stop-loss-fn       (or stop-loss-fn default-stop-loss-fn)
       :take-profit-fn     (or take-profit-fn default-take-profit-fn)
       :trailing-stop-fn   (or trailing-stop-fn default-trailing-stop-fn)
       :trailing-profit-fn (or trailing-profit-fn default-trailing-profit-fn)
       :break-even-fn      (or break-even-fn default-break-even-fn)})))


(extend-type Strategy
  PStrategy
  (name
    [strategy]
    (:name strategy))
  (look-for-open-trade
    ([{:keys [open-trade-fn]} data i]
     (open-trade-fn strategy data i))
    ([strategy data]
     (look-for-open-trade strategy data 0)))
  (look-for-close-trade
    ([{:keys [close-trade-fn] :as strategy} trade data i]
     (close-trade-fn strategy trade data i))
    ([strategy trade data]
     (look-for-close-trade strategy trade data 0)))
  (stop-loss
    ([{:keys [stop-loss-fn] :as strategy} side data i]
     (stop-loss-fn strategy side data i))
    ([strategy side data]
     (stop-loss strategy side data 0)))
  (take-profit
    ([{:keys [take-profit-fn] :as strategy} side data i]
     (take-profit-fn strategy side data i))
    ([strategy side data]
     (take-profit strategy side data 0)))
  (trailing-stop
    ([{:keys [trailing-stop-fn] :as strategy} trade data i]
     (trailing-stop-fn strategy trade data i))
    ([strategy side data]
     (trailing-stop strategy side data 0)))
  (trailing-profit
    ([{:keys [trailing-profit-fn] :as strategy} trade data i]
     (trailing-profit-fn strategy trade data i))
    ([strategy side data]
     (trailing-profit strategy side data 0)))
  (break-even
    ([{:keys [break-even-fn] :as strategy} trade data i]
     (break-even-fn strategy trade data i))
    ([strategy side data]
     (break-even strategy side data 0)))
  (prepare-trade
    ([{:keys [account] :as strategy}
      {:keys [instrument period ^doubles open-bid# ^doubles open-ask#
              ^doubles close-bid# ^doubles close-ask#] :as data} i]
     (when-let [side (look-for-open-trade strategy data i)]
       (let [instrument-pip (oanda/instrument-pip account instrument)
             open-time (if (zero? i)
                         (tim/next-candle-time period (aget ^objects (:time# data) i))
                         (aget ^objects (:time# data) (dec i)))
             open-price (case side
                          :buy (if (zero? i) (aget close-ask# i) (aget open-ask# (dec i)))
                          :sell (if (zero? i) (aget close-bid# i) (aget open-bid# (dec i))))
             stop-loss-price (stop-loss strategy side data i)
             stop-loss-pips (/ (case side
                                 :buy (- stop-loss-price open-price)
                                 :sell (- open-price stop-loss-price)) instrument-pip)
             take-profit-price (take-profit strategy side data i)
             take-profit-pips (/ (case side
                                   :buy (- take-profit-price open-price)
                                   :sell (- open-price take-profit-price)) instrument-pip)]
         {:strategy          (:name strategy)
          :instrument        instrument
          :period            period
          :side              side
          :open-time         open-time
          :open-price        open-price
          :stop-loss-price   stop-loss-price
          :stop-loss-pips    stop-loss-pips
          :take-profit-price take-profit-price
          :take-profit-pips  take-profit-pips})))
    ([strategy data]
     (prepare-trade strategy data 0)))
  (history-trades
    [{:keys [account] :as strategy}
     {:keys [^objects time# ^doubles high-bid# ^doubles high-ask#
             ^doubles low-bid# ^doubles low-ask# ^doubles close-bid#
             ^doubles close-ask# ^doubles spread#] :as data}
     {:keys [trailing-stop? trailing-profit? break-even?] :as opts}]
    (let [{:keys [max-spread-percent] :as strategy} (merge strategy opts)
          instrument (:instrument data)
          instrument-pip (oanda/instrument-pip account instrument)
          instrument-tick-value (oanda/instrument-tick-value account instrument)
          loop-limit (dec (alength time#))]
      (loop [i 0 trades (transient [])]
        (if (< i loop-limit)
          (if-let [{:keys [side open-price stop-loss-price stop-loss-pips
                           take-profit-price take-profit-pips]
                    :as   tmp-trade} (prepare-trade strategy data i)]
            (if (and (permission/spread-allowed? (aget spread# i) (- (* stop-loss-pips instrument-pip max-spread-percent))))
              (let [open-candle-nr (dec i)
                   tmp-trade (assoc tmp-trade
                               :instrument-pip instrument-pip
                               :instrument-tick-value instrument-tick-value
                               :open-candle-nr open-candle-nr
                               :entry-stop-loss-price stop-loss-price
                               :entry-stop-loss-pips stop-loss-pips
                               :stop-loss-pips stop-loss-pips
                               :take-profit-price take-profit-price
                               :take-profit-pips take-profit-pips
                               :duration nil)
                   trade (loop [j 1 {:keys [stop-loss-price stop-loss-pips
                                            take-profit-price take-profit-pips]
                                     :as   t} tmp-trade]
                           (if (<= j i)
                             (let [current-candle (- i j)
                                   current-time (aget time# current-candle)]
                               (cond
                                 ; reached stop-loss-price
                                 (case side
                                   :buy (<= (aget low-bid# current-candle) stop-loss-price)
                                   :sell (>= (aget high-ask# current-candle) stop-loss-price))
                                 (assoc t
                                   :close-time current-time
                                   :close-candle-nr current-candle
                                   :duration j
                                   :close-price stop-loss-price
                                   :pips-profit stop-loss-pips
                                   :close-type :stop-loss)

                                 ; reached take-profit-price
                                 (case side
                                   :buy (>= (aget high-bid# current-candle) take-profit-price)
                                   :sell (<= (aget low-ask# current-candle) take-profit-price))
                                 (assoc t
                                   :close-time current-time
                                   :close-candle-nr current-candle
                                   :duration j
                                   :close-price take-profit-price
                                   :pips-profit take-profit-pips
                                   :close-type :take-profit)

                                 ; reached profit
                                 (look-for-close-trade strategy t data current-candle)
                                 (assoc t
                                   :close-time current-time
                                   :close-candle-nr current-candle
                                   :duration j
                                   :close-price (case side
                                                  :buy (aget close-bid# current-candle)
                                                  :sell (aget close-ask# current-candle))
                                   :pips-profit (case side
                                                  :buy (- (aget close-bid# current-candle) open-price)
                                                  :sell (- open-price (aget close-ask# current-candle)))
                                   :close-type :close-trade)

                                 ; trade continue
                                 :else
                                 (case side
                                   :buy (let [break-even-price (when break-even?
                                                                 (break-even strategy t data current-candle))
                                              trailing-stop-price (when trailing-stop?
                                                                    (trailing-stop strategy t data current-candle))
                                              trailing-profit-price (when trailing-profit?
                                                                      (trailing-profit strategy t data current-candle))]
                                          (let [trailing-stop-pips (when trailing-stop-price
                                                                     (/ (- trailing-stop-price open-price) instrument-pip))
                                                trailing-profit-pips (when trailing-profit-price
                                                                       (/ (- trailing-profit-price open-price) instrument-pip))]
                                            (recur (inc j)
                                                   (cond-> t
                                                           (and break-even? break-even-price)
                                                           (assoc :stop-loss-price break-even-price)
                                                           (and trailing-stop? trailing-stop-price)
                                                           (assoc :stop-loss-price trailing-stop-price
                                                                  :stop-loss-pips trailing-stop-pips)
                                                           (and trailing-profit? trailing-profit-price)
                                                           (assoc :take-profit-price trailing-profit-price
                                                                  :take-profit-pips trailing-profit-pips)))))
                                   :sell (let [break-even-price (when break-even?
                                                                  (break-even strategy t data current-candle))
                                               trailing-stop-price (when trailing-stop?
                                                                     (trailing-stop strategy t data current-candle))
                                               trailing-profit-price (when trailing-profit?
                                                                       (trailing-profit strategy t data current-candle))]
                                           (let [trailing-stop-pips (when trailing-stop-price
                                                                      (/ (- open-price trailing-stop-price) instrument-pip))
                                                 trailing-profit-pips (when trailing-profit-price
                                                                        (/ (- open-price trailing-profit-price) instrument-pip))]
                                             (recur (inc j)
                                                    (cond-> t
                                                            (and break-even? break-even-price)
                                                            (assoc :stop-loss-price break-even-price)
                                                            (and trailing-stop? trailing-stop-price)
                                                            (assoc :stop-loss-price trailing-stop-price
                                                                   :stop-loss-pips trailing-stop-pips)
                                                            (and trailing-profit? trailing-profit-price)
                                                            (assoc :take-profit-price trailing-profit-price
                                                                   :take-profit-pips trailing-profit-pips))))))))
                             (assoc t
                               :close-time nil
                               :close-candle-nr nil
                               :duration j
                               :close-price (case side
                                              :buy (aget close-bid# (inc (- i j)))
                                              :sell (aget close-ask# (inc (- i j))))
                               :pips-profit (case side
                                              :buy (/ (- (aget close-bid# (inc (- i j))) open-price) instrument-pip)
                                              :sell (/ (- open-price (aget close-ask# (inc (- i j)))) instrument-pip))
                               :close-type nil)))]
               (recur (inc i) (conj! trades trade)))
              (recur (inc i) trades))
            (recur (inc i) trades))
          (persistent! trades))))))