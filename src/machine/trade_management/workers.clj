(ns machine.trade-management.workers
  (:require [clojure.core.async :refer [chan go-loop >! <! alts! sub timeout close!]]
            [mount.core :refer [defstate]]
            [schema.core :as s]
            [clj-time.core :as dt]
            [taoensso.timbre :as timbre]
            [machine.settings.core :refer [settings]]
            [machine.db.core :refer [DB]]
            [machine.router.core :as router]
            [machine.oanda.core :as oanda]
            [machine.time-management.market-hours :as market-hours]
            [machine.trade-management.core :refer :all]
            [machine.trade-management.io :as io])
  (:import
    [machine.oanda.core OandaAccount]))


(s/defn start-trades-info-worker
  [{:keys [account-id] :as account} :- OandaAccount]
  (let [open-trade-event (chan)]
    (sub router/publication :open-trade open-trade-event)
    (go-loop []
      (if (market-hours/within-forex-hours? (dt/now))
        (when-let [[{:keys [trade]} p] (alts! [open-trade-event (timeout 1000)])]
          (let [trades-info (oanda/trades-info account)
                trades-db (get-in @DB [:open-trades account-id])]
            (if (= p open-trade-event)
              (do
                (timbre/info "[trades-info-worker] - found trade" (:id trade))
                (if-not (some #(= (:id trade) (:id %)) trades-db)
                  (when (add-trade-to-db! trade)
                    (timbre/info "[trades-info-worker] - add trade" (:id trade)
                                 "to trade-db successful"))
                  (timbre/info "[trades-info-worker] -" (:id trade) "is already in the database")))
              (do
                (timbre/debug "[trades-info-worker] - not found any trades")
                (doseq [{:keys [id] :as trade} trades-db]
                  (when-not (some #(= id (:id %)) trades-info)
                    (timbre/info "[trades-info-worker] - remove trade" id "from trade-db successful")
                    ;(io/add-trade-to-history! trade)
                    (remove-trade-from-db! trade)))))))
        (do (timbre/error "[trades-info-worker] -  outside workdays hours!")
            (<! (timeout 60000))))
      (recur))))


(declare trades-info-worker)
(defstate trades-info-worker
          :start (mapv start-trades-info-worker oanda/oanda-accounts)
          :stop (mapv close! trades-info-worker))