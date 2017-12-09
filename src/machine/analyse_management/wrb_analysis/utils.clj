(ns machine.analyse-management.wrb-analysis.utils
  (:require [machine.quotes.series :refer :all]
            [criterium.core :refer [bench quick-bench]])
  (:import [machine.quotes.core Quotes]
           [clojure.lang PersistentVector]
           [org.apache.commons.math3.util FastMath]))


(defn generate-keyword
  [prefix main sufix]
  (keyword (str (name (or prefix "")) (name main) (name (or sufix "")))))


(defn zone-keyword
  [main]
  (generate-keyword nil main "#"))


(defn open-keyword
  [main]
  (generate-keyword nil main "-open#"))

(defn close-keyword
  [main]
  (generate-keyword nil main "-close#"))

(defn size-keyword
  [main]
  (generate-keyword nil main "-size#"))

(defn contraction-count-keyword
  [main]
  (generate-keyword nil main "-contraction-count#"))


(defn contraction-size-keyword
  [main]
  (generate-keyword nil main "-contraction-size#"))

(defn confirmation-candle-keyword
  [main]
  (generate-keyword nil main "-confirmation-candle#"))

(defn filled-by-keyword
  [main]
  (generate-keyword nil main "-filled-by#"))

(defn type-keyword
  [main]
  (generate-keyword nil main "-type#"))

(defn inside-keyword
  [main]
  (generate-keyword nil main "-inside#"))

(defn inside-nr-keyword
  [main]
  (generate-keyword nil main "-inside-nr#"))

(defn prior-bull-keyword
  [main] (generate-keyword nil main "-prior-bull-nr#"))

(defn prior-bear-keyword
  [main] (generate-keyword nil main "-prior-bull-nr#"))

(defn next-bull-keyword
  [main] (generate-keyword nil main "-next-bull-nr#"))

(defn next-bear-keyword
  [main] (generate-keyword nil main "-next-bull-nr#"))



;(defn contraction-share?
;  [^long v1 ^long v2 ^Quotes data]
;  (let [^doubles open# (:open data)
;        ^doubles high# (:high data)
;        ^doubles low# (:low data)
;        ^doubles close# (:close data)]
;    (println (> (FastMath/max (aget open# v2) (aget close# v2))
;                (amin (aslice high# (inc v1) v2))
;                (FastMath/min (aget open# v2) (aget close# v2))))
;    (println (FastMath/max (aget open# v2) (aget close# v2))
;             (amin (aslice high# (inc v1) v2))
;             (FastMath/min (aget open# v2) (aget close# v2)))
;    (and (< v1 v2)
;         (or (> (FastMath/max (aget open# v2) (aget close# v2))
;                (amin (aslice high# (inc v1) v2))
;                (FastMath/min (aget open# v2) (aget close# v2)))
;             (> (FastMath/max (aget open# v2) (aget close# v2))
;                (amax (aslice low# (inc v1) v2))
;                (FastMath/min (aget open# v2) (aget close# v2)))))))


(defn identity-union
  ^longs [^PersistentVector arrays]
  (let [array-size (alength ^longs (first arrays))
        r (long-array array-size)]
    (loop [[a1 & arr] arrays]
      (when a1
        (loop [i 0]
          (when (< i array-size)
            (let [val (aget ^longs a1 i)]
              (when (and (not (zero? val)) (zero? (aget r i)))
                (aset r i val)))
            (recur (inc i))))
        (recur arr)))
    r))


(defn object-union
  ^objects [^PersistentVector arrays]
  (let [array-size (alength ^objects (first arrays))
        r (object-array array-size)]
    (loop [[a1 & arr] arrays]
      (when a1
        (loop [i 0]
          (when (< i array-size)
            (let [val (aget ^objects a1 i)]
              (when (and (not (nil? val)) (nil? (aget r i)))
                (aset r i val)))
            (recur (inc i))))
        (recur arr)))
    r))


(defn price-union
  ^doubles [^PersistentVector arrays]
  (let [array-size (alength ^doubles (first arrays))
        r (double-array array-size)]
    (loop [[a1 & arr] arrays]
      (when a1
        (loop [i 0]
          (when (< i array-size)
            (when (zero? (aget r i))
              (aset r i (aget ^doubles a1 i)))
            (recur (inc i))))
        (recur arr)))
    r))


(defn size-union
  ^doubles [^PersistentVector arrays]
  (let [array-size (alength ^doubles (first arrays))
        r (double-array array-size)]
    (loop [[a1 & arr] arrays]
      (when a1
        (loop [i 0]
          (when (< i array-size)
            (when (zero? (aget r i))
              (aset r i (aget ^doubles a1 i)))
            (recur (inc i))))
        (recur arr)))
    r))


(defn contraction-share?
  [^long v1 ^long v2 ^Quotes {:keys [^doubles high# ^doubles low#]}]
  (loop [i 1]
    (if (< v1 (- v2 i))
      (if (or (< (aget high# v2) (aget low# (- v2 i)))
              (> (aget low# v2) (aget high# (- v2 i))))
        false
        (recur (inc i)))
      true)))


;(definline ^boolean contraction-share? [v1 v2 high# low#]
;  `(if (< v1 v2)
;     (loop [i# 1]
;       (if (< i# (- ~v2 ~v1))
;         (if (or (< (aget (doubles ~high#) ~v2) (aget (doubles ~high#) (unchecked-subtract-int ~v2 i#)))
;                 (> (aget (doubles ~low#) ~v2) (aget (doubles ~low#) (unchecked-subtract-int ~v2 i#))))
;           false
;           (recur (unchecked-inc-int i#)))
;         true))
;     false))


(defn contraction-size-break?
  [^long v1 ^long v2 ^Quotes {:keys [^doubles candle-body-size#]}]
  (> (FastMath/min (aget candle-body-size# v1) (aget candle-body-size# v2))
     (amax (aslice candle-body-size# (inc v1) v2))))


(defn contraction-break-bull?
  [^long v1 ^long v2 ^Quotes {:keys [high# close#] :as data}]
  (and (> (aget close# v1)
          (amax (aslice high# (inc v1) v2)))
       (contraction-size-break? v1 v2 data)))


(defn contraction-break-bear?
  [^long v1 ^long v2 ^Quotes {:keys [low# close#] :as data}]
  (and (< (aget close# v1)
          (amin (aslice low# (inc v1) v2)))
       (contraction-size-break? v1 v2 data)))
