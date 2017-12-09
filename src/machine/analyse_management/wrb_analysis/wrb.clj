(ns machine.analyse-management.wrb-analysis.wrb
  (:require [machine.quotes.series :refer :all]
            [criterium.core :refer [bench quick-bench]])
  (:import [machine.quotes.core Quotes]))


(set! *unchecked-math* true)


(defprotocol Wrb
  (wrb-body [data] [data i])
  (prior-bull-wrb-body [data] [data i])
  (prior-bear-wrb-body [data] [data i])
  (prior-unfilled-bull-wrb-body [data] [data i])
  (prior-unfilled-bear-wrb-body [data] [data i])
  (next-bull-wrb-body [data] [data i])
  (next-bear-wrb-body [data] [data i])
  (wrb-hg-body [data] [data i])
  (prior-bull-wrb-hg-body [data] [data i])
  (prior-bear-wrb-hg-body [data] [data i])
  (prior-unfilled-bull-wrb-hg-body [data] [data i])
  (prior-unfilled-bear-wrb-hg-body [data] [data i])
  (next-bull-wrb-hg-body [data] [data i])
  (next-bear-wrb-hg-body [data] [data i])
  (fade-volatility [data] [data i])
  (volatility-spike [data] [data i]))

(extend-type Quotes
  Wrb
  (wrb-body
    (^Quotes [{:keys [^objects time# ^longs candle-direction#
                      ^longs candle-broken-body-size#] :as data}]
     (let [array-size (alength time#)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (when (>= (aget candle-broken-body-size# i) 3)
             (aset result i (aget candle-direction# i)))
           (recur (inc i))))
       (assoc data :wrb-body# result)))
    (^long [{:keys [^longs wrb-body#] :as data} ^long i]
     (if wrb-body#
       (aget wrb-body# i)
       (recur (wrb-body# data) i))))
  (wrb-hg-body
    (^Quotes [{:keys [^objects time# ^doubles candle-hidden-gap# ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           result (long-array array-size)]
       (loop [i 1]
         (when (< i array-size)
           (when (and (not= 0 (aget wrb-body# i))
                      (pos? (aget candle-hidden-gap# i)))
             (aset result i (aget wrb-body# i)))
           (recur (inc i))))
       (assoc data :wrb-hg-body# result)))
    (^long [{:keys [^longs wrb-hg-body#] :as data} ^long i]
     (if wrb-hg-body#
       (aget wrb-hg-body# i)
       (recur (wrb-hg-body data) i))))
  (prior-bull-wrb-body
    (^Quotes [{:keys [^objects time# ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (== 1 (aget wrb-body# (+ i j)))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :prior-bull-wrb# result)))
    (^long [{:keys [^longs prior-bull-wrb#] :as data} ^long i]
     (if prior-bull-wrb#
       (aget prior-bull-wrb# i)
       (recur (prior-bull-wrb-body data) i))))
  (prior-bear-wrb-body
    (^Quotes [{:keys [^objects time# ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (== -1 (aget wrb-body# (+ i j)))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :prior-bear-wrb# result)))
    (^long [{:keys [^longs prior-bear-wrb#] :as data} ^long i]
     (if prior-bear-wrb#
       (aget prior-bear-wrb# i)
       (recur (prior-bear-wrb-body data) i))))
  (prior-unfilled-bull-wrb-body
    (^Quotes [{:keys [^objects time# ^longs candle-filled-by# ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (and (== 1 (aget wrb-body# (+ i j)))
                        (< (aget candle-filled-by# (+ i j)) i))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :prior-unfilled-bull-wrb# result)))
    (^long [{:keys [^longs prior-unfilled-bull-wrb#] :as data} ^long i]
     (if prior-unfilled-bull-wrb#
       (aget prior-unfilled-bull-wrb# i)
       (recur (prior-unfilled-bull-wrb-body data) i))))
  (prior-unfilled-bear-wrb-body
    (^Quotes [{:keys [^objects time# ^longs candle-filled-by# ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (and (== -1 (aget wrb-body# (+ i j)))
                        (< (aget candle-filled-by# (+ i j)) i))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :prior-unfilled-bear-wrb# result)))
    (^long [{:keys [^longs prior-unfilled-bear-wrb#] :as data} ^long i]
     (if prior-unfilled-bear-wrb#
       (aget prior-unfilled-bear-wrb# i)
       (recur (prior-unfilled-bear-wrb-body data) i))))
  (next-bull-wrb-body
    (^Quotes [{:keys [^objects time# ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           ^longs result (long-array array-size)]
       (loop [i 1]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< j i)
               (if (== 1 (aget wrb-body# (- i j)))
                 (aset result i (- i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :next-bull-wrb# result)))
    (^long [{:keys [^longs next-bull-wrb#] :as data} ^long i]
     (if next-bull-wrb#
       (aget next-bull-wrb# i)
       (recur (next-bull-wrb-body data) i))))
  (next-bear-wrb-body
    (^Quotes [{:keys [^objects time# ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           ^longs result (long-array array-size)]
       (loop [i 1]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< j i)
               (if (== -1 (aget wrb-body# (- i j)))
                 (aset result i (- i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :next-bear-wrb# result)))
    (^long [{:keys [^longs next-bear-wrb#] :as data} ^long i]
     (if next-bear-wrb#
       (aget next-bear-wrb# i)
       (recur (next-bear-wrb-body data) i))))
  (prior-bull-wrb-hg-body
    (^Quotes [{:keys [^objects time# ^longs wrb-hg-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (== 1 (aget wrb-hg-body# (+ i j)))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :prior-bull-wrb-hg# result)))
    (^long [{:keys [^longs prior-bull-wrb-hg#] :as data} ^long i]
     (if prior-bull-wrb-hg#
       (aget prior-bull-wrb-hg# i)
       (recur (prior-bull-wrb-hg-body data) i))))
  (prior-bear-wrb-hg-body
    (^Quotes [{:keys [^objects time# ^longs wrb-hg-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (== -1 (aget wrb-hg-body# (+ i j)))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :prior-bear-wrb-hg# result)))
    (^long [{:keys [^longs prior-bear-wrb-hg#] :as data} ^long i]
     (if prior-bear-wrb-hg#
       (aget prior-bear-wrb-hg# i)
       (recur (prior-bear-wrb-hg-body data) i))))
  (prior-unfilled-bull-wrb-hg-body
    (^Quotes [{:keys [^objects time# ^longs candle-filled-by# ^longs wrb-hg-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (and (== 1 (aget wrb-hg-body# (+ i j)))
                        (< (aget candle-filled-by# (+ i j)) i))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :prior-unfilled-bull-wrb-hg# result)))
    (^long [{:keys [^longs prior-unfilled-bull-wrb-hg#] :as data} ^long i]
     (if prior-unfilled-bull-wrb-hg#
       (aget prior-unfilled-bull-wrb-hg# i)
       (recur (prior-unfilled-bull-wrb-hg-body data) i))))
  (prior-unfilled-bear-wrb-hg-body
    (^Quotes [{:keys [^objects time# ^longs candle-filled-by# ^longs wrb-hg-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (and (== -1 (aget wrb-hg-body# (+ i j)))
                        (< (aget candle-filled-by# (+ i j)) i))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :prior-unfilled-bear-wrb-hg# result)))
    (^long [{:keys [^longs prior-unfilled-bear-wrb-hg#] :as data} ^long i]
     (if prior-unfilled-bear-wrb-hg#
       (aget prior-unfilled-bear-wrb-hg# i)
       (recur (prior-unfilled-bear-wrb-hg-body data) i))))
  (next-bull-wrb-hg-body
    (^Quotes [{:keys [^objects time# ^longs wrb-hg-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           ^longs result (long-array array-size)]
       (loop [i 1]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< j i)
               (if (== 1 (aget wrb-hg-body# (- i j)))
                 (aset result i (- i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :next-bull-wrb-hg# result)))
    (^long [{:keys [^longs next-bull-wrb-hg#] :as data} ^long i]
     (if next-bull-wrb-hg#
       (aget next-bull-wrb-hg# i)
       (recur (next-bull-wrb-hg-body data) i))))
  (next-bear-wrb-hg-body
    (^Quotes [{:keys [^objects time# ^longs wrb-hg-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           ^longs result (long-array array-size)]
       (loop [i 1]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< j i)
               (if (== -1 (aget wrb-hg-body# (- i j)))
                 (aset result i (- i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :next-bear-wrb-hg# result)))
    (^long [{:keys [^longs next-bear-wrb-hg#] :as data} ^long i]
     (if next-bear-wrb-hg#
       (aget next-bear-wrb-hg# i)
       (recur (next-bear-wrb-hg-body data) i))))
  (fade-volatility
    (^Quotes [{:keys [^objects time# ^doubles close# ^longs candle-direction#
                      ^doubles candle-body-midpoint# ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (case (aget candle-direction# i)
             1 (when (and (== -1 (aget wrb-body# (inc i)))
                          (>= (aget close# i)
                              (aget candle-body-midpoint# (inc i))))
                 (aset result i 1))

             -1 (when (and (== 1 (aget wrb-body# (inc i)))
                           (<= (aget close# i)
                               (aget candle-body-midpoint# (inc i))))
                  (aset result i -1))
             0 nil)
           (recur (inc i))))
       (assoc data :wrb-fade-volatility# result)))
    (^long [{:keys [^longs wrb-fade-volatility#] :as data} ^long i]
     (if wrb-fade-volatility#
       (aget wrb-fade-volatility# i)
       (recur (fade-volatility data) i))))
  (volatility-spike
    (^Quotes [{:keys [^objects time# ^longs candle-direction# ^longs candle-broken-range#
                      ^longs wrb-body#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (case (aget candle-direction# i)
             1 (when (and (== -1 (aget wrb-body# i))
                          (>= (aget candle-broken-range# i) 3))
                 (aset result i 1))

             -1 (when (and (== 1 (aget wrb-body# i))
                           (>= (aget candle-broken-range# i) 3))
                  (aset result i -1))
             0 nil)
           (recur (inc i))))
       (assoc data :wrb-volatility-spike# result)))
    (^long [{:keys [^longs wrb-volatility-spike#] :as data} ^long i]
     (if wrb-volatility-spike#
       (aget wrb-volatility-spike# i)
       (recur (volatility-spike data) i)))))


(defn analyse
  ^Quotes [^Quotes data]
  (-> data
      (wrb-body)
      (prior-bull-wrb-body)
      (prior-bear-wrb-body)
      (prior-unfilled-bull-wrb-body)
      (prior-unfilled-bear-wrb-body)
      (next-bull-wrb-body)
      (next-bear-wrb-body)
      (wrb-hg-body)
      (prior-bull-wrb-hg-body)
      (prior-bear-wrb-hg-body)
      (prior-unfilled-bull-wrb-hg-body)
      (prior-unfilled-bear-wrb-hg-body)
      (next-bull-wrb-hg-body)
      (next-bear-wrb-hg-body)
      (fade-volatility)
      (volatility-spike)))


;(defn analyse-profile
;  ^Quotes [data]
;  (taoensso.timbre.profiling/p :wrb-body# (wrb-body# data))
;  (taoensso.timbre.profiling/p :hg-body (hg-body data))
;  (taoensso.timbre.profiling/p :prior-wrb-body# (prior-wrb-body# data))
;  (taoensso.timbre.profiling/p :next-wrb-body# (next-wrb-body# data))
;  (taoensso.timbre.profiling/p :prior-unfilled-wrb-body# (prior-unfilled-wrb-body# data))
;  (taoensso.timbre.profiling/p :prior-hg-body (prior-hg-body data))
;  (taoensso.timbre.profiling/p :next-hg-body (next-hg-body data))
;  (taoensso.timbre.profiling/p :prior-unfilled-hg-body (prior-unfilled-hg-body data))
;  (taoensso.timbre.profiling/p :fade-volatility (fade-volatility data)))


