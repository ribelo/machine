(ns machine.analyse-management.seiden.core
  (:require [machine.quotes.series :refer :all]
            [machine.analyse-management.wrb-analysis.wrb :as wrb])
  (:import [clojure.lang Keyword]
           [machine.quotes.core Quotes]
           [org.apache.commons.math3.util FastMath]))


(defprotocol SupplyDemand
  (base [data n m])
  (rally [data n m]))

(extend-type Quotes
  SupplyDemand
  (base
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles close#
                      ^doubles high# ^doubles low# ^doubles candle-body-size#
                      ^longs wrb-body#] :as data}
              ^long n ^long m]
     (let [array-size (alength time#)
           console# (boolean-array array-size)
           console-count# (long-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (loop [j m]
             (when (< (+ i j) array-size)
               (when (>= j n)
                 (if (and (<= (aget close# i) (aget high# (+ i j)))
                          (>= (aget close# i) (aget low# (+ i j)))
                          (zero? (aget wrb-body# (+ i j))))
                   (do (aset console# i true)
                       (aset console-count# i j))
                   (recur (dec j))))))
           (recur (inc i))))
       (assoc data :sd-base# console#
                   :sd-base-count# console-count#))))
  (rally
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles close#
                      ^doubles high# ^doubles low# ^doubles candle-body-size#
                      ^longs wrb-body#] :as data}
              ^long n ^long m]
     (let [array-size (alength time#)
           console# (boolean-array array-size)
           console-count# (long-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (loop [j m]
             (when (< (+ i j) array-size)
               (when (>= j n)
                 (if (and (<= (aget close# i) (aget high# (+ i j)))
                          (>= (aget close# i) (aget low# (+ i j)))
                          (zero? (aget wrb-body# (+ i j))))
                   (do (aset console# i true)
                       (aset console-count# i j))
                   (recur (dec j))))))
           (recur (inc i))))
       (assoc data :sd-base# console#
                   :sd-base-count# console-count#)))))