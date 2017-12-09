(ns machine.oanda.workers
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
            [machine.db.core :refer [DB] :as db]
            [machine.router.core :as router]
            [machine.oanda.core :as oanda]
            [machine.quotes.core :as q]
            [machine.time-management.core :as tim]
            [machine.time-management.market-hours :as market-hours])
  (:import [machine.oanda.core OandaAccount]))


(s/defn start-account-info-worker
  [{:keys [account-id] :as account} :- OandaAccount]
  (timbre/info "[account-info] -" [account-id] "- starting worker!")
  (go-loop []
    (if (market-hours/within-forex-hours? (dt/now))
      (when-let [info (oanda/account-info account)]
        (when-let [[_ p] (alts! [[router/channel {:event :update-account-info
                                                  :account-id account-id
                                                  :data  info}]
                                 (timeout 10000)])]
          (if (= p router/channel)
            (<! (timeout 1000))
            (timbre/error "[account-info-worker] -" [account-id]
                          "-  can't put to the channel"))))
      (do (timbre/error "[account-info-worker] -" [account-id]
                        "-  outside workdays hours!")
          (<! (timeout 60000))))
    (recur)))


(s/defn start-accounts-worker
  [{:keys [account-id] :as account} :- OandaAccount]
  (timbre/info "[accounts-info] -" [account-id] " starting worker!")
  (go-loop []
    (if (market-hours/within-forex-hours? (dt/now))
      (when-let [info (oanda/accounts-info account)]
        (when-let [[_ p] (alts! [[router/channel {:event :update-accounts-info
                                                  :account-id account-id
                                                  :data  info}]
                                 (timeout 10000)])]
          (if (= p router/channel)
            (<! (timeout 10000))
            (timbre/error "[accounts-info-worker] -" [account-id]
                          "- can't put to the channel"))))
      (do (timbre/error "[accounts-info-worker] -" [account-id]
                        "-  outside workdays hours!")
          (<! (timeout 60000))))
    (recur)))


(s/defn start-prices-info-worker
  [{:keys [account-id] :as account} :- OandaAccount]
  (timbre/info "[prices-info] -" [account-id] "- starting worker!")
  (go-loop []
    (if (market-hours/within-forex-hours? (dt/now))
      (when-let [info (oanda/prices-info account)]
        (when-let [[_ p] (alts! [[router/channel {:event      :update-prices-info
                                                  :account-id account-id
                                                  :data       info}]
                                 (timeout 5000)])]
          (if (= p router/channel)
            (<! (timeout 1000))
            (timbre/error "[prices-info-worker] -" [account-id]
                          "- can't put to the channel"))))
      (do (timbre/error "[prices-info-worker] -" [account-id]
                        "- outside workdays hours!")
          (<! (timeout 60000))))
    (recur)))


(s/defn start-instruments-info-worker
  [{:keys [account-id] :as account} :- OandaAccount]
  (timbre/info "[instruments-info] -" [account-id] "- starting worker!")
  (go-loop []
    (if (market-hours/within-forex-hours? (dt/now))
      (let [info (oanda/instruments-info account)]
        (when-let [[_ p] (alts! [[router/channel {:event      :update-instruments-info
                                                  :account-id account-id
                                                  :data       info}]
                                 (timeout 10000)])]
          (if (= p router/channel)
            (<! (timeout 5000))
            (timbre/error "[instruments-info-worker] -" [account-id]
                          "- can not put to the channel"))))
      (do (timbre/error "[instruments-info-worker] -" [account-id]
                        "-  outside workdays hours!")
          (<! (timeout 60000))))
    (recur)))


(declare account-info-worker)
(defstate account-info-worker
          :start (mapv start-account-info-worker oanda/oanda-accounts)
          :stop (mapv close! account-info-worker))


(declare accounts-info-worker)
(defstate accounts-info-worker
          :start (mapv start-accounts-worker oanda/oanda-accounts)
          :stop (mapv close! accounts-info-worker))


(declare prices-info-workers)
(defstate prices-info-workers
          :start (mapv (fn [{:keys [account-id] :as account}]
                         {account-id (start-prices-info-worker account)})
                       oanda/oanda-accounts)
          :stop (mapv close! prices-info-workers))


(declare instruments-info-workers)
(defstate instruments-info-workers
          :start (mapv (fn [{:keys [account-id] :as account}]
                         {account-id (start-instruments-info-worker account)})
                       oanda/oanda-accounts)
          :stop (mapv close! instruments-info-workers))