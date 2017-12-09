(ns machine.data-management.workers
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
            [machine.settings.core :as settings]
            [machine.db.core :refer [DB] :as db]
            [machine.router.core :as router]
            [machine.oanda.core :as oanda]
            [machine.quotes.core :as q]
            [machine.time-management.core :as tim]
            [machine.time-management.market-hours :as market-hours]))


(s/defn start-update-quotes-workers
  [{:keys [account-id] :as account}]
  (let [partitioned-instruments (partition-all (round (/ (count dm/instruments) 4))
                                               dm/instruments)]
    (->> partitioned-instruments
         (mapv (fn [instruments]
                 (let [tick-event (chan 1 (comp (filter #((set instruments) (:instrument %)))))]
                   (sub router/publication :new-tick tick-event)
                   (let [worker
                         (go-loop []
                           (when-let [[v p] (alts! [tick-event (timeout 60000)])]
                             (if (= p tick-event)
                               (let [{received-time :time :keys [instrument]} v]
                                 (if (market-hours/within-trading-hours? instrument (dt/now))
                                   (doseq [period dm/periods]
                                     (let [received-candle-time (tim/previous-candle-time period received-time)]
                                       (if-let [stored-quotes (db/get-quotes instrument period)]
                                         (let [stored-time (q/last-candle-time stored-quotes)]
                                           (when (dt/after? received-candle-time stored-time)
                                             (timbre/info "[update-quotes-worker] -" [instrument period]
                                                          "- received candle time" (dtc/to-string received-candle-time))
                                             (timbre/info "[update-quotes-worker] -" [instrument period]
                                                          "- stored time" (dtc/to-string stored-time))
                                             (timbre/info "[update-quotes-worker] -" [instrument period]
                                                          "- received candle time is newer than stored time")
                                             (when-let [updated-quotes (dm/update-quotes account stored-quotes)]
                                               (db/save-quotes! instrument period updated-quotes)
                                               (timbre/info "[update-quotes-worker] -" [instrument period]
                                                            "- updated quotes successful")
                                               (when-let [[_ p] (alts! [[router/channel {:event      :update-quotes
                                                                                         :account-id account-id
                                                                                         :instrument instrument
                                                                                         :period     period
                                                                                         :data       updated-quotes
                                                                                         :time       received-candle-time}]
                                                                        (timeout 10000)])]
                                                 (when-not (= p router/channel)
                                                   (timbre/error "[update-quotes-worker] - can't put to the channel"))))))
                                         (do
                                           (timbre/info "[update-quotes-worker] -" [instrument period]
                                                        "- quotes not exists")
                                           (when-let [downloaded-quotes (dm/download-quotes account instrument period)]
                                             (db/save-quotes! instrument period downloaded-quotes)
                                             (timbre/info "[update-quotes-worker] -" [instrument period]
                                                          "- download quotes successful")
                                             (when-let [[_ p] (alts! [[router/channel {:event      :update-quotes
                                                                                       :account-id account-id
                                                                                       :instrument instrument
                                                                                       :period     period
                                                                                       :data       downloaded-quotes
                                                                                       :time       received-candle-time}]
                                                                      (timeout 10000)])]
                                               (when-not (= p router/channel)
                                                 (timbre/error "[update-quotes-worker] -" [instrument period]
                                                               "can't put to the channel"))))))))
                                   (do (timbre/error "[update-quotes-worker] -" (vec instruments) "- outside trading hours!")
                                       (<! (timeout 60000)))))
                               (timbre/error "[update-quotes-worker] -" (vec instruments) "- can't take tick from the channel")))
                           (recur))]
                     {:instruments (vec instruments)
                      :account-id  account-id
                      :worker      worker})))))))


(declare update-quotes-worker)
(defstate update-quotes-worker
          :start (start-update-quotes-workers (first oanda/oanda-accounts))
          :stop (doseq [{:keys [worker]} update-quotes-worker]
                  (close! worker)))
