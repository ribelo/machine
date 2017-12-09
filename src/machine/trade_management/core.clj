(ns machine.trade-management.core
  (:require [schema.core :as s]
            [taoensso.timbre :as timbre]
            [machine.db.core :refer [DB]]
            [machine.utils :refer :all]
            [machine.time-management.core :as tim]
            [machine.oanda.core :as oanda]
            [machine.oanda.schema :refer :all])
  (:import (machine.oanda.core OandaAccount)))


(s/defn add-trade-to-db!
  [trade :- {s/Keyword s/Any}]
  (swap! DB update :open-trades conj trade))


(s/defn remove-trade-from-db!
  [{:keys [id]} :- {s/Keyword s/Any}]
  (swap! DB update :open-trades
         (fn [coll] (into [] (remove #(= id (:id %)) coll)))))


(s/defn update-trade-in-db!
  [trade :- {s/Keyword s/Any}]
  (remove-trade-from-db! trade)
  (add-trade-to-db! trade))


(s/defn instrument-invested? :- (s/maybe s/Bool)
  [account :- OandaAccount instrument :- s/Keyword]
  (when-let [trades-info (oanda/trades-info account)]
    (->> trades-info
         (map :instrument)
         (some #{instrument})
         (some?))))


(s/defn oposite-direction-trade? :- (s/maybe s/Bool)
  ([first-instrument first-side second-instrument second-side]
    (let [first-split (condp = first-side
                        :buy (split-instrument first-instrument)
                        :sell (reverse (split-instrument first-instrument)))
          second-split (condp = second-side
                         :buy (split-instrument second-instrument)
                         :sell (reverse (split-instrument second-instrument)))]
      (when (every? #(== 3 (count (name %))) (concat first-split second-split))
        (or (= (first first-split) (second second-split))
            (= (second first-split) (first second-split))))))
  ([{first-instrument :instrument first-side :side} :- TradeSchema
    {second-instrument :instrument second-side :side} :- TradeSchema]
    (oposite-direction-trade? first-instrument first-side second-instrument second-side)))


(s/defn same-direction-trade? :- (s/maybe s/Bool)
  ([first-instrument first-side second-instrument second-side]
    (let [first-split (condp = first-side
                        :buy (split-instrument first-instrument)
                        :sell (reverse (split-instrument first-instrument)))
          second-split (condp = second-side
                         :buy (split-instrument second-instrument)
                         :sell (reverse (split-instrument second-instrument)))]
      (when (every? #(== 3 (count (name %))) (concat first-split second-split))
        (or (= (first first-split) (first second-split))
            (= (second first-split) (second second-split))))))
  ([{first-instrument :instrument first-side :side} :- TradeSchema
    {second-instrument :instrument second-side :side} :- TradeSchema]
    (same-direction-trade? first-instrument first-side second-instrument second-side)))


(s/defn oposit-direction-trades :- (s/maybe [TradeSchema])
  ([account :- OandaAccount instrument :- s/Keyword side :- (s/enum :buy :sell)]
    (when-let [trades-info (oanda/trades-info account)]
      (->> trades-info
           (filterv (fn [{second-instrument :instrument second-side :side}]
                      (oposite-direction-trade? instrument side second-instrument second-side)))))))


(s/defn same-direction-trades :- (s/maybe [TradeSchema])
  ([account :- OandaAccount instrument :- s/Keyword side :- (s/enum :buy :sell)]
    (when-let [trades-info (oanda/trades-info account)]
      (->> trades-info
           (filterv (fn [{second-instrument :instrument second-side :side}]
                      (same-direction-trade? instrument side second-instrument second-side)))))))


(s/defn close-oposit-direction-trades
  ([account :- OandaAccount instrument :- s/Keyword period :- s/Keyword side :- (s/enum :buy :sell)]
    (let [oposit-trades (->> (oposit-direction-trades account instrument side)
                             (filter (fn [{oposit-trade-period :period}]
                                       (or (nil? oposit-trade-period)
                                           (= period oposit-trade-period)
                                           (tim/greater-period? period oposit-trade-period)))))]
      (if-not (empty? oposit-trades)
        (doseq [{oposit-id :id oposit-instrument :instrument oposit-period :period}
                oposit-trades]
          (when (oanda/close-trade! account oposit-id)
            (timbre/info "[trade-management] -" [instrument period side]
                         "- close oposit trade successfull - "
                         [oposit-instrument oposit-period oposit-id]))))
      (timbre/info "[trade-management] -" [instrument period side]
                   "- no relevant oposit transactions to close")))
  ([account :- OandaAccount {:keys [instrument period side]}]
    (close-oposit-direction-trades account instrument period side)))