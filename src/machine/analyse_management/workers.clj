(ns machine.analyse-management.workers
  (:require [clojure.core.async :refer [chan sub go-loop close! <! >! alts! timeout]]
            [clojure.math.combinatorics :refer [cartesian-product combinations]]
            [mount.core :refer [defstate]]
            [schema.core :as s]
            [clj-time.core :as dt]
            [clj-time.coerce :as dtc]
            [taoensso.timbre :as timbre]
            [taoensso.encore :refer [dissoc-in path assoc-some round reset-in! when-lets]]
            [machine.utils :refer :all]
            [machine.settings.core :refer [settings]]
            [machine.quotes.series :refer :all]
            [machine.db.core :refer [DB] :as db]
            [machine.router.core :as router]
            [machine.time-management.core :as tim]
            [machine.analyse-management.quantum.correlation :refer [pearsons-correlation]]
            [machine.analyse-management.core :refer :all]
            [machine.analyse-management.wrb-analysis.fvb :as fvb]
            [machine.analyse-management.wrb-analysis.vtr :as vtr]
            [machine.data-management.core :as dm :refer [instruments periods]]
            [machine.oanda.core :as oanda])
  (:import [clojure.core.async.impl.channels ManyToManyChannel]))


(s/defn start-analyse-workers :- [{:instrument s/Keyword
                                   :period     s/Keyword
                                   :worker     ManyToManyChannel}] ;FIXME
  []
  (->> (cartesian-product dm/instruments dm/periods)
       (mapv (fn [[instrument period]]
               (timbre/info "[base-analyse-worker] -" [instrument period]
                            "- starting worker!")
               (let [wait-time (* (tim/period->seconds period) 1.1 1000)
                     update-quotes-event (chan 1 (comp (filter #(and (= instrument (:instrument %))
                                                                     (= period (:period %))))))]
                 (sub router/publication :update-quotes update-quotes-event)
                 (let [worker (go-loop []
                                (when-let [[v p] (alts! [update-quotes-event (timeout wait-time)])]
                                  (if (= p update-quotes-event)
                                    (when-let [{:keys [data time]} v]
                                      (timbre/info "[base-analyse-worker] -" [instrument period]
                                                   "- going to analyse quotes")
                                      (when-let [analysed (analyse-quotes data)]
                                        (timbre/info "[base-analyse-worker] -" [instrument period]
                                                     "- analysed quotes successful")
                                        (db/save-quotes! instrument period analysed)
                                        (when-let [[_ p] (alts! [[router/channel {:event      :analyse/quotes
                                                                                  :instrument instrument
                                                                                  :period     period
                                                                                  :data       analysed
                                                                                  :time       time}]
                                                                 (timeout 10000)])]
                                          (when-not (= p router/channel)
                                            (timbre/error "[base-analyse-worker] -" [instrument period]
                                                          "can't put to the channel")))))
                                    (timbre/error "[base-analyse-worker] -" [instrument period]
                                                  "- can't take data from the channel"))
                                  (recur)))]
                   {:instrument instrument
                    :period     period
                    :worker     worker}))))))



(s/defn start-correlation-analyse-worker
  []
  (let [wait-time (-> (sort-by tim/period->seconds dm/periods)
                      (first)
                      (tim/period->seconds)
                      (* 1.1 1000))
        analyse-period-event (chan)]
    (sub router/publication :analyse/period analyse-period-event
         (go-loop []
           (when-let [[v p] (alts! [analyse-period-event (timeout wait-time)])]
             (if (= p analyse-period-event)
               (let [{all-data :data :keys [period time]} v]
                 (timbre/info "[correlation-analyse-worker] -" [period]
                              "- going to analyse instruments")
                 (doseq [main-data all-data]
                   (let [main-instrument (:instrument main-data)
                         analysed (->> all-data
                                       (remove #(= main-instrument (:instrument %)))
                                       (reduce (fn [d1 d2]
                                                 (pearsons-correlation d1 d2 1024))
                                               main-data))]
                     (timbre/info "[correlation-analyse-worker] -"
                                  [main-instrument period]
                                  "- analysed quotes successful")
                     (when-let [[_ p] (alts! [[router/channel {:event      :analyse/correlation
                                                               :instrument main-instrument
                                                               :period     period
                                                               :main-data  analysed
                                                               :rest-data  (->> all-data
                                                                                (remove #(= main-instrument (:instrument %))))
                                                               :time       time}]
                                              (timeout 10000)])]
                       (when-not (= p router/channel)
                         (timbre/error "[correlation-analyse-worker] -" [main-instrument period]
                                       "can't put to the channel"))))))
               (timbre/error "[correlation-analyse-worker] - can't take data from the channel"))
             (recur))))))


(declare analyse-quotes-workers)
(defstate analyse-quotes-workers
          :start (start-analyse-workers)
          :stop (doseq [{:keys [worker]} analyse-quotes-workers]
                  (close! worker)))


(declare correlation-analyse-worker)
(defstate correlation-analyse-worker
          :start (start-correlation-analyse-worker)
          :stop (close! correlation-analyse-worker))