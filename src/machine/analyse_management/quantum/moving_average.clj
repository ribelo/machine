(ns machine.analyse-management.quantum.moving-average
  (:refer-clojure :exclude [amap])
  (:require [machine.quotes.series :refer :all]
            [machine.analyse-management.quantum.utils :refer :all]))


(defn simple-moving-average [data ^long length]
  (assoc data (keyword (str "sma-" length)) (sma (:close data) length)))
