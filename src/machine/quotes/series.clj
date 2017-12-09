(ns machine.quotes.series
  (:refer-clojure :exclude [amap areduce])
  (:require [criterium.core :refer [quick-bench bench]])
  (:import [java.util Arrays]
           [org.apache.commons.math3.stat StatUtils]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)


(defprotocol PSeries
  ;(amap [arr f])
  ;(areduce [arr f])
  (afirst [arr])
  (alast [arr])
  (aslice [arr start stop] [arr start])
  (atake [arr n])
  (atake-last [arr n])
  (asum [arr])
  (amean [arr])
  (amin [arr])
  (amin-idx [arr])
  (amax [arr])
  (amax-idx [arr])
  (aevery [arr pred])
  (asome [arr pred])
  (index-of [arr val])
  (last-index-of [arr val]))

(into-array [:a :b])


(extend-type (Class/forName "[D")
  PSeries
  ;(amap [arr f]
  ;  (let [r (double-array (alength ^doubles arr))]
  ;    (loop [i 0]
  ;      (when (< i (alength ^doubles arr))
  ;        (aset r i ^double (f (aget ^doubles arr i)))
  ;        (recur (unchecked-inc-int i))))
  ;    r))
  (afirst [arr]
          (aget ^doubles arr 0))
  (alast [arr]
         (aget ^doubles arr (unchecked-dec-int (alength ^doubles arr))))
  (aslice
    ([arr start stop]
      (Arrays/copyOfRange ^doubles arr (int start) (int stop)))
    ([arr start]
      (Arrays/copyOfRange ^doubles arr (int start) (dec (alength ^doubles arr)))))
  (atake
    [arr n]
    (Arrays/copyOfRange ^doubles arr (int 0) (int n)))
  (atake-last
    [arr n]
    (Arrays/copyOfRange ^doubles arr ^int (dec (- (alength ^doubles arr) n)) (alength ^doubles arr)))
  (asum [arr]
        (StatUtils/sum arr))
  (amean [arr]
         (StatUtils/mean arr))
  (amin [arr]
        (StatUtils/min arr))
  (amin-idx [arr]
            (loop [i (int 1) val (aget ^doubles arr 0) idx (int 0)]
              (if (< i (alength ^doubles arr))
                (recur (unchecked-inc-int i)
                       (Math/min val (aget ^doubles arr i))
                       (if (< (aget ^doubles arr i) val)
                         i idx))
                idx)))
  (amax [arr]
        (StatUtils/max arr))
  (amax-idx [arr]
            (loop [i (int 1) val (aget ^doubles arr 0) idx (int 0)]
              (if (< i (alength ^doubles arr))
                (recur (unchecked-inc-int i)
                       (Math/max val (aget ^doubles arr i))
                       (if (> (aget ^doubles arr i) val)
                         i idx))
                idx)))
  (aevery [arr pred]
          (loop [i (int 0)]
            (if (< i (alength ^doubles arr))
              (if ^boolean (pred (aget ^doubles arr i))
                (recur (unchecked-inc-int i))
                false)
              true)))
  (asome [arr pred]
         (loop [i (int 0)]
           (if (< i (alength ^doubles arr))
             (if-not ^boolean (pred (aget ^doubles arr i))
               (recur (unchecked-inc-int i))
               true)
             false)))
  ;(sma [arr length]
  ;     (let [r (double-array (alength ^doubles arr))]
  ;       (loop [i (int 0)]
  ;         (when (< i (alength ^doubles arr))
  ;           (aset r i (double (amean (aslice arr (Math/max (int 0)
  ;                                                           (int (- (inc i) length)))
  ;                                            (inc i)))))
  ;           (recur (unchecked-inc-int i))))
  ;       r))
  )


(extend-type (Class/forName "[J")
  PSeries
  ;(amap [arr f]
  ;  (let [r (long-array (alength ^longs arr))]
  ;    (loop [i 0]
  ;      (when (< i (alength ^longs arr))
  ;        (aset r i ^long (f (aget ^longs arr i)))
  ;        (recur (unchecked-inc-int i))))
  ;    r))
  (afirst [arr]
          (aget ^longs arr 0))
  (alast [arr]
         (aget ^longs arr (unchecked-dec-int (alength ^longs arr))))
  (aslice
    ([arr start stop]
      (Arrays/copyOfRange ^longs arr (int start) (int stop)))
    ([arr start]
      (Arrays/copyOfRange ^longs arr (int start) (alength ^longs arr))))
  (atake
    [arr n]
    (Arrays/copyOfRange ^longs arr (int 0) (int n)))
  (atake-last
    [arr n]
    (Arrays/copyOfRange ^longs arr ^int (dec (- (alength ^longs arr) n)) (alength ^longs arr)))
  (asum [arr]
        (loop [i (int 0) sum 0]
          (if (< i (alength ^longs arr))
            (recur (unchecked-inc-int i) (+ sum (aget ^longs arr i)))
            sum)))
  (amean [arr]
         (/ (asum arr) (alength ^longs arr)))
  (amin [arr]
        (loop [i (int 1) val (aget ^longs arr 0)]
          (if (< i (alength ^longs arr))
            (recur (unchecked-inc-int i) (Math/min val (aget ^longs arr i)))
            val)))
  (amin-idx [arr]
            (loop [i (int 1) val (aget ^longs arr 0) idx (int 0)]
              (if (< i (alength ^longs arr))
                (recur (unchecked-inc-int i)
                       (Math/min val (aget ^longs arr i))
                       (if (< (aget ^longs arr i) val)
                         i idx))
                idx)))
  (amax [arr]
        (loop [i (int 1) val (aget ^longs arr 0)]
          (if (< i (alength ^longs arr))
            (recur (unchecked-inc-int i) (Math/max val (aget ^longs arr i)))
            val)))
  (amax-idx [arr]
            (loop [i (int 1) val (aget ^longs arr 0) idx (int 0)]
              (if (< i (alength ^longs arr))
                (recur (unchecked-inc-int i)
                       (Math/max val (aget ^longs arr i))
                       (if (> (aget ^longs arr i) val)
                         i idx))
                idx)))
  (aevery [arr pred]
          (loop [i (int 0)]
            (if (< i (alength ^longs arr))
              (if ^boolean (pred (aget ^longs arr i))
                (recur (unchecked-inc-int i))
                false)
              true)))
  (asome [arr pred]
         (loop [i (int 0)]
           (if (< i (alength ^longs arr))
             (if-not ^boolean (pred (aget ^longs arr i))
               (recur (unchecked-inc-int i))
               true)
             false))))


(extend-type (Class/forName "[Z")
  PSeries
  ;(amap [arr f]
  ;  (let [r (boolean-array (alength ^booleans arr))]
  ;    (loop [i 0]
  ;      (when (< i (alength ^booleans arr))
  ;        (aset r i ^boolean (f (aget ^booleans arr i)))
  ;        (recur (unchecked-inc-int i))))
  ;    r))
  (afirst [arr]
          (aget ^booleans arr 0))
  (alast [arr]
         (aget ^booleans arr (unchecked-dec-int (alength ^booleans arr))))
  (aslice
    ([arr start stop]
      (Arrays/copyOfRange ^booleans arr (int start) (int stop)))
    ([arr start]
      (Arrays/copyOfRange ^booleans arr (int start) (alength ^booleans arr))))
  (atake
    [arr n]
    (Arrays/copyOfRange ^booleans arr (int 0) (int n)))
  (atake-last
    [arr n]
    (Arrays/copyOfRange ^booleans arr ^int (dec (- (alength ^booleans arr) n)) (alength ^booleans arr)))
  (aevery [arr pred]
          (loop [i (int 0)]
            (if (< i (alength ^booleans arr))
              (if ^boolean (pred (aget ^booleans arr i))
                (recur (unchecked-inc-int i))
                false)
              true)))
  (asome [arr pred]
         (loop [i (int 0)]
           (if (< i (alength ^booleans arr))
             (if-not ^boolean (pred (aget ^booleans arr i))
               (recur (unchecked-inc-int i))
               true)
             false))))

(extend-type (class (object-array 0))
  PSeries
  (afirst [arr]
          (aget ^objects arr 0))
  (alast [arr]
         (aget ^objects arr (unchecked-dec-int (alength ^objects arr))))
  (aslice
    ([arr start stop]
      (Arrays/copyOfRange ^objects arr (int start) (int stop)))
    ([arr start]
      (Arrays/copyOfRange ^objects arr (int start) (alength ^booleans arr))))
  (atake
    [arr n]
    (Arrays/copyOfRange ^objects arr (int 0) (int n)))
  (atake-last
    [arr n]
    (Arrays/copyOfRange ^objects arr ^int (dec (- (alength ^objects arr) n)) (alength ^objects arr)))
  (aevery [arr pred]
          (loop [i (int 0)]
            (if (< i (alength ^objects arr))
              (if ^boolean (pred (aget ^objects arr i))
                (recur (unchecked-inc-int i))
                false)
              true)))
  (asome [arr pred]
         (loop [i (int 0)]
           (if (< i (alength ^objects arr))
             (if-not ^boolean (pred (aget ^objects arr i))
               (recur (unchecked-inc-int i))
               true)
             false))))


(extend-type (Class/forName "[Lorg.joda.time.DateTime;")
  PSeries
  (afirst [arr]
          (aget #^"[Lorg.joda.time.DateTime;" arr 0))
  (alast [arr]
         (aget #^"[Lorg.joda.time.DateTime;" arr (unchecked-dec-int (alength ^booleans arr))))
  (aslice
    ([arr start stop]
      (Arrays/copyOfRange #^"[Lorg.joda.time.DateTime;" arr (int start) (int stop)))
    ([arr start]
      (Arrays/copyOfRange #^"[Lorg.joda.time.DateTime;" arr (int start) (alength ^booleans arr))))
  (atake
    [arr n]
    (Arrays/copyOfRange #^"[Lorg.joda.time.DateTime;" arr (int 0) (int n)))
  (atake-last
    [arr n]
    (Arrays/copyOfRange #^"[Lorg.joda.time.DateTime;" arr ^int (dec (- (alength #^"[Lorg.joda.time.DateTime;" arr) n)) (alength #^"[Lorg.joda.time.DateTime;" arr)))
  (aevery [arr pred]
          (loop [i (int 0)]
            (if (< i (alength #^"[Lorg.joda.time.DateTime;" arr))
              (if ^boolean (pred (aget #^"[Lorg.joda.time.DateTime;" arr i))
                (recur (unchecked-inc-int i))
                false)
              true)))
  (asome [arr pred]
         (loop [i (int 0)]
           (if (< i (alength #^"[Lorg.joda.time.DateTime;" arr))
             (if-not ^boolean (pred (aget #^"[Lorg.joda.time.DateTime;" arr i))
               (recur (unchecked-inc-int i))
               true)
             false)))
  (index-of [arr val]
            (let [array-size (alength #^"[Lorg.joda.time.DateTime;" arr)]
              (loop [i (int 0)]
                (if (< i array-size)
                  (if (= val (aget #^"[Lorg.joda.time.DateTime;" arr i))
                    i
                    (recur (unchecked-inc-int i)))
                  -1)))))

