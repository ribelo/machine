(ns machine.analyse-management.quantum.volatility
  (:refer-clojure :exclude [amap])
  (:require [machine.quotes.series :refer :all]
            [machine.analyse-management.quantum.utils :refer :all])
  (:import [machine.quotes.core Quotes]
           [org.apache.commons.math3.util FastMath]))


(defn atr [data ^long length]
  (let [^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        array-size (alength open#)
        result (double-array array-size)]
    (loop [i (dec (dec array-size))]
      (when (>= i 0)
        (aset result i (max (- (aget high# i)
                               (aget low# i))
                            (Math/abs (- (aget close# (inc i))
                                         (aget high# i)))
                            (Math/abs (- (aget close# (inc i))
                                         (aget low# i)))))
        (recur (dec i))))
    (assoc data :atr# (sma result length))))


(defn efficiency-ratio
  ^Quotes [^Quotes {:keys [^doubles close#]
                    :as   data} ^long n]
  (let [array-size (alength close#)
        result# (double-array array-size)]
    (loop [i (dec (- array-size n))]
      (when (>= i 0)
        (let [change (- (aget close# i) (aget close# (+ i n)))
              volatility (loop [j 0 r 0]
                           (if (< j n)
                             (recur (inc j) (+ r (FastMath/abs ^double (- (aget close# (+ i j))
                                                                          (aget close# (inc (+ i j)))))))
                             r))
              ratio (/ change volatility)]
          (aset result# i ratio))
        (recur (dec i))))
    (assoc data :efficiency-ratio# result#)))