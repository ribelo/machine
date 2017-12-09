(ns machine.analyse-management.core
  (:require [schema.core :as s]
            [machine.analyse-management.quantum.volatility :refer [atr]]
            [machine.analyse-management.quantum.moving-average :refer [simple-moving-average]]
            [machine.analyse-management.candle.core :as candle]
            [machine.analyse-management.wrb-analysis.wrb :as wrb]
            [machine.analyse-management.wrb-analysis.confirmation :as confirmation]
            [machine.analyse-management.wrb-analysis.zone :as zone]
            [machine.analyse-management.wrb-analysis.dcm :as dcm]
            [machine.analyse-management.wrb-analysis.vsa :as vsa])
  (:import [machine.quotes.core Quotes]))


(s/defn analyse-quotes :- Quotes
  [quotes :- Quotes]
  (-> quotes
      (atr 20)
      (candle/analyse)
      ;(candle/upper-period-direction :d)
      ;(candle/upper-period-direction :m)
      (wrb/analyse)
      (confirmation/analyse 16)
      (zone/analyse 16)
      (vsa/volume-average 20)
      (dcm/dcm)))



