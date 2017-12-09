(ns machine.time-management.workers
  (:require [clojure.core.async :refer [chan go-loop >! <! alts! sub timeout close!]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [mount.core :refer [defstate]]
            [schema.core :as s]
            [clj-time.core :as dt]
            [clj-time.coerce :as dtc]
            [taoensso.timbre :as timbre]
            [taoensso.encore :refer [dissoc-in path assoc-some round reset-in! when-lets]]
            [machine.data-management.core :as dm]
            [machine.utils :refer :all]
            [machine.settings.core :refer [settings]]
            [machine.db.core :refer [DB] :as db]
            [machine.router.core :as router]
            [machine.oanda.core :as oanda]
            [machine.quotes.core :as q]
            [machine.time-management.core :as tim]
            [machine.time-management.market-hours :as market-hours])
  (:import [clojure.core.async.impl.channels ManyToManyChannel]
           [machine.oanda.core OandaAccount]))


(s/defn start-tick-watchman
  []
  (timbre/info "[tick-watchman] - starting worker!")
  (let [prices-event (chan)
        cache (atom {})]
    (sub router/publication :update-prices-info prices-event)
    (go-loop []
      (if (market-hours/within-forex-hours? (dt/now))
        (let [instruments dm/instruments]
          (when-let [[v p] (alts! [prices-event (timeout 5000)])]
            (if (= p prices-event)
              (doseq [instrument instruments]
                (when-let [{received-time :time
                            :keys         [bid ask halted]} (->> v
                                                                 :data
                                                                 (filter #(= instrument (:instrument %)))
                                                                 (first))]
                  (doseq [period dm/periods]
                    (let [cached-time (get-in @cache [instrument period])]
                      (timbre/debug "[tick-watchman] -" [instrument period]
                                    "- last cached time" (dtc/to-string cached-time))
                      (timbre/debug "[tick-watchman] -" [instrument period]
                                    "- received time" (dtc/to-string received-time))
                      (when (or (nil? cached-time) (dt/after? received-time cached-time))
                        (timbre/debug "[tick-watchman] -" [instrument period]
                                      "- received time is newer than cached")
                        (swap! cache assoc-in [instrument period] received-time)
                        (when-let [[_ p] (alts! [[router/channel {:event      :new-tick
                                                                  :instrument instrument
                                                                  :time       received-time
                                                                  :bid        bid
                                                                  :ask        ask
                                                                  :halted?    halted}]
                                                 (timeout 1000)])]
                          (when-not (= p router/channel)
                            (timbre/error "[tick-watchman] - can't put to the channel"))))))))
              (timbre/error "[tick-watchman] - can't take prices from the channel"))))
        (do (timbre/error "[tick-watchman] -  outside workdays hours!")
            (<! (timeout 60000))))
      (recur))))

(s/defn start-candle-watchman
  []
  (timbre/info "[candle-watchman] - starting worker!")
  (let [tick-event (chan)
        cache (atom {})]
    (sub router/publication :new-tick tick-event)
    (go-loop []
      (if (market-hours/within-forex-hours? (dt/now))
        (when-let [[v p] (alts! [tick-event (timeout 5000)])]
          (if (= p tick-event)
            (doseq [instrument dm/instruments]
              (let [{received-time :time :keys [bid]} v]
                (doseq [period dm/periods]
                  (let [cached-time (get-in @cache [instrument period])]
                    (timbre/debug "[candle-watchman] -" [instrument period]
                                  "- last cached time" (dtc/to-string cached-time))
                    (timbre/debug "[candle-watchman] -" [instrument period]
                                  "- last server time" (dtc/to-string received-time))
                    (when (or (nil? cached-time) (dt/after? received-time cached-time))
                      (timbre/debug "[candle-watchman] -" [instrument period]
                                    "- received time is newer than cached")
                      (swap! cache assoc-in [instrument period] (tim/candle-time period received-time))
                      (when-let [[_ p] (alts! [[router/channel {:event      :new-candle
                                                                :instrument instrument
                                                                :time       received-time
                                                                :open       bid}]
                                               (timeout 1000)])]
                        (when-not (= p router/channel)
                          (timbre/error "[candle-watchman] - can't put to the channel"))))))))
            (timbre/error "[candle-watchman] - can't take prices from the channel")))
        (do (timbre/error "[candle-watchman] -  outside forex hours!")
            (<! (timeout 60000))))
      (recur))))


(s/defn start-period-watchmen
  [account :- OandaAccount]
  (let [wait-time (-> (sort-by tim/period->seconds dm/periods)
                      (first)
                      (tim/period->seconds)
                      (* 1.1 1000))
        analyse-event (chan)
        quotes-queue (atom (reduce (fn [m [period instrument]]
                                     (assoc-in m [period instrument] {:time nil :data nil}))
                                   {} (cartesian-product dm/periods dm/instruments)))]
    (sub router/publication :analyse/quotes analyse-event)
    (go-loop []
      (when-let [[v p] (alts! [analyse-event (timeout wait-time)])]
        (if (= p analyse-event)
          (let [{:keys [instrument period time data]} v
                cached-time (get-in @quotes-queue [period instrument :time])]
            (when (or (nil? cached-time) (dt/after? time cached-time))
              (swap! quotes-queue assoc-in [period instrument] {:time time :data data})
              (let [analysed-count (->> (get @quotes-queue period)
                                        (map #(get-in % [1 :time]))
                                        (map #(= time %))
                                        (filter true?)
                                        (count))
                    instruments-count (if (> (count (oanda/instruments-active account dm/instruments)) 0)
                                        (count (oanda/instruments-active account dm/instruments))
                                        (count dm/instruments))]
                (timbre/info "[analyse-period-watchmen] -" [instrument period]
                             "- received time" (dtc/to-string time))
                (timbre/info "[analyse-period-watchmen] -" [instrument period]
                             "count" analysed-count "should count" instruments-count)
                (if (>= analysed-count instruments-count)
                  (do
                    (timbre/info "[analyse-period-watchmen] -" [period]
                                 "- analysed period successful")
                    (when-let [[_ p] (alts! [[router/channel {:event       :analyse/period
                                                              :period      period
                                                              :instruments (keys (get @quotes-queue period))
                                                              :time        time
                                                              :data        (->> (get @quotes-queue period)
                                                                                (map #(get-in % [1 :data])))}]

                                             (timeout 10000)])]
                      (when-not (= p router/channel)
                        (timbre/error "[analyse-period-watchmen] -" [instrument period]
                                      "can't put to the channel"))))))))
          (timbre/error "[analyse-period-watchmen] - can't take data from the channel"))
        (recur)))))


(declare tick-watchman)
(defstate tick-watchman
          :start (start-tick-watchman)
          :stop (close! tick-watchman))


(declare candle-watchman)
;(defstate candle-watchman
;          :start (start-candle-watchman)
;          :stop (close! candle-watchman))


(declare period-watchmen)
(defstate period-watchmen
          :start (start-period-watchmen (first oanda/oanda-accounts))
          :stop (close! period-watchmen))