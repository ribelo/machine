(ns machine.data-management.core
  (:require [mount.core :refer [defstate]]
            [schema.core :as s]
            [taoensso.timbre :as timbre]
            [clj-time.core :as dt]
            [machine.utils :refer :all]
            [machine.settings.core :as settings]
            [machine.quotes.core :as q]
            [machine.quotes.series :refer :all]
            [machine.db.core :as db :refer [DB]]
            [machine.oanda.core :as oanda]
            [machine.time-management.core :as tim]
            [machine.settings.core :as settings])
  (:import [machine.quotes.core Quotes]
           [machine.oanda.core OandaAccount]))


(declare instruments)
(defstate instruments
          :start (vec (settings/all-instruments))
          :stop [])


(declare periods)
(defstate periods
          :start (vec (settings/all-periods))
          :stop [])


(s/defn download-quotes :- (s/maybe Quotes)
  ([account :- OandaAccount instrument :- s/Keyword period :- s/Keyword candles-count :- s/Num]
    (if (> candles-count 5000)
      (let [seconds (tim/period->seconds period)
            to-time (tim/candle-time period)
            from-time (tim/floor-time (dt/minus to-time
                                                (dt/seconds (* seconds candles-count)))
                                      (tim/period->seconds :d))]
        (timbre/debug "[download-quotes] -" [instrument period] "- download candles data from"
                      from-time "to" to-time)
        (when-let [data (oanda/quotes-history account instrument period from-time to-time)]
          (timbre/debug "[download-quotes] -" [instrument period] "- download candles data successful")
          (q/fill-gaps (q/from-rows data))))
      (when-let [data (oanda/quotes-history account instrument period candles-count)]
        (timbre/debug "[download-quotes] -" [instrument period] "- download candles data successful")
        (q/fill-gaps (q/from-rows data)))))
  ([account :- OandaAccount instrument :- s/Keyword period :- s/Keyword]
    (download-quotes account instrument period 4096))
  ([instrument :- s/Keyword period :- s/Keyword]
    (let [account (oanda/create-oanda-account (first (settings/all-oanda-accounts)))]
      (download-quotes account instrument period 4096))))


(s/defn update-quotes :- (s/maybe Quotes)
  ([account :- OandaAccount {:keys [instrument period] :as quotes} :- Quotes]
    (let [from-time (q/last-candle-time quotes)
          to-time (tim/candle-time period)]

      (when (dt/after? to-time from-time)
        (timbre/debug "[update-quotes] -" [instrument period] "download candles data from"
                      from-time "to" to-time)
        (if-let [new-quotes (oanda/quotes-history account instrument period from-time to-time)]
          (do
            (timbre/debug "[update-quotes] -" [instrument period]
                          "- download candles data successful")
            (-> quotes
                (q/cleanup)
                (->> (q/join (q/from-rows new-quotes)))
                (q/fill-gaps)))
          (timbre/debug "[update-quotes] -" [instrument period]
                        "- downloaded data is empty!")))))
  ([account :- OandaAccount instrument :- s/Keyword period :- s/Keyword]
    (when-let [quotes (db/get-quotes instrument period)]
      (update-quotes account quotes))))
