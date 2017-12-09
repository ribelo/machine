(ns machine.analyse-management.quantum.utils
  (:refer-clojure :exclude [amap])
  (:require [machine.quotes.series :refer :all]
            [criterium.core :refer [bench quick-bench]]))


(defn sma [data ^long length]
  (let [array-length (alength data)
        result (double-array array-length)]
    (loop [i 0]
      (when (< i array-length)
        (aset result i ^double (amean (aslice data (max 0 (- (inc i) length)) (inc i))))
        (recur (inc i))))
    result))


(defn ema [^doubles data ^long length]
  (let [array-size (alength data)
        result (double-array array-size)]
    (loop [i (- array-size length 2)]
      (when (>= i 0)
        (let [price (aget data i)
              prev (aget data (inc i))
              r (+ (* (/ 2.0 (inc length))
                      (- price prev))
                   prev)]
          (aset result i r)
         (recur (dec i)))))
    result))
