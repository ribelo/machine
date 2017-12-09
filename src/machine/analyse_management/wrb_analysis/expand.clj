(ns machine.analyse-management.wrb-analysis.expand
  (:require [machine.quotes.series :refer :all]
            [criterium.core :refer [bench quick-bench]])
  (:import [machine.quotes.core Quotes]
           [org.apache.commons.math3.util FastMath]))


(defn weak-expand-bull-after
  [^long i ^Quotes data]
  (let [^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs filled-by# (:candle-filled-by# data)]
    (loop [j 1 cnt 0]
      (if (> (- i j) (aget filled-by# i))
        (cond
          (>= cnt 2) (- i j)

          (and (== 1 (aget direction# (- i j)))
               (> (aget close# (- i j)) (aget close# i)))
          (recur (inc j) (inc cnt))

          :else (recur (inc j) cnt))))))


(defn weak-expand-bear-after
  [^long i ^Quotes data]
  (let [^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs filled-by# (:candle-filled-by# data)]
    (loop [j 1 cnt 0]
      (if (> (- i j) (aget filled-by# i))
        (cond
          (>= cnt 2) (- i j)

          (and (== -1 (aget direction# (- i j)))
               (< (aget close# (- i j)) (aget close# i)))
          (recur (inc j) (inc cnt))

          :else (recur (inc j) cnt))))))


(defn weak-expand-bull-before
  [^long i ^Quotes data]
  (let [^doubles high (:high# data)
        ^doubles close (:close# data)
        ^longs direction (:candle-direction# data)
        loop-limit (FastMath/min (long 8) (- (alength high) i))]
    (loop [j 1 cnt 0]
      (if (< j loop-limit)
        (cond
          (>= cnt 2) (+ i j)
          (> (aget close (+ i j)) (aget high i)) nil

          (== 1 (aget direction (+ i j)))
          (recur (inc j) (inc cnt))

          :else (recur (inc j) cnt))))))


(defn weak-expand-bear-before
  [^long i ^Quotes data]
  (let [^doubles low (:low# data)
        ^doubles close (:close# data)
        ^longs direction (:candle-direction# data)
        loop-limit (FastMath/min (long 8) (- (alength low) i))]
    (loop [j 1 cnt 0]
      (if (< j loop-limit)
        (cond
          (>= cnt 2) (+ i j)
          (< (aget close (+ i j)) (aget low i)) nil

          (== -1 (aget direction (+ i j)))

          (recur (inc j) (inc cnt))

          :else (recur (inc j) cnt))))))


;(defmacro ^boolean bull-after-weak? [i ^doubles open# ^doubles low#
;                                     ^doubles close#
;                                     ^longs direction#
;                                     & {:keys [look-distance]
;                                        :or   {look-distance 13}}]
;  `(let [loop-limit# (min look-distance i)]
;     (loop [j# 1 cnt# 0]
;       (if (< j# loop-limit#)
;         (or
;           (when (> cnt# 2)
;             true)
;           (if (and (== 1 (aget direction# (- i j#)))
;                    (> (aget close# (- i j#))
;                       (aget close# i)))
;             (recur (unchecked-inc-int j#) (unchecked-inc cnt#))
;             (recur (unchecked-inc-int j#) cnt#)))
;         false))))


;(defmacro ^boolean bull-before-weak? [i ^doubles open# ^doubles low#
;                                      ^doubles close#
;                                      ^longs direction#
;                                      & {:keys [look-distance]
;                                         :or   {look-distance 13}}]
;  `(let [array-size# (alength (doubles open#))
;         loop-limit# (min look-distance (- array-size# i))]
;     (loop [j# 1 cnt# 0]
;       (if (< j# loop-limit#)
;         (or
;           (when (> cnt# 2)
;             true)
;           (if (and (== 1 (aget direction# (+ i j#)))
;                    (< (aget close# (+ i j#))
;                       (aget close# i)))
;             (recur (unchecked-inc-int j#) (unchecked-inc cnt#))
;             (recur (unchecked-inc-int j#) cnt#)))
;         false))))


;(defmacro ^boolean bear-after-weak? [i ^doubles open# ^doubles low#
;                                     ^doubles close# ^longs direction#
;                                     & {:keys [look-distance]
;                                        :or   {look-distance 13}}]
;  `(let [loop-limit# (min look-distance i)]
;     (loop [j# 1 cnt# 0]
;       (if (< j# loop-limit#)
;         (or
;           (when (> cnt# 2)
;             true)
;           (if (and (== -1 (aget direction# (- i j#)))
;                    (< (aget close# (- i j#))
;                       (aget close# i)))
;             (recur (unchecked-inc-int j#) (unchecked-inc cnt#))
;             (recur (unchecked-inc-int j#) cnt#)))
;         false))))


;(defmacro ^boolean bear-before-weak? [i ^doubles open# ^doubles low#
;                                      ^doubles close# ^longs direction#
;                                      & {:keys [look-distance]
;                                         :or   {look-distance 13}}]
;  `(let [array-size# (alength (doubles open#))
;         loop-limit# (min look-distance (- array-size# i))]
;     (loop [j# 1 cnt# 0]
;       (if (< j# loop-limit#)
;         (or
;           (when (> cnt# 2)
;             true)
;           (if (and (== -1 (aget direction# (+ i j#)))
;                    (> (aget close# (+ i j#))
;                       (aget close# i)))
;             (recur (unchecked-inc-int j#) (unchecked-inc cnt#))
;             (recur (unchecked-inc-int j#) cnt#)))
;         false))))


(defn bull-consecutive-direction
  [^long i ^long count ^long distance ^Quotes {:keys [candle-consecutive-direction#]}]
  (loop [k 3]
    (when (and (<= k (+ distance 3)) (>= (- i k) 0))
      (if (>= (aget candle-consecutive-direction# (- i k)) count)
        (- i k)
        (recur (inc k))))))


(defn bear-consecutive-direction
  [^long i ^long count ^long distance ^Quotes {:keys [candle-consecutive-direction#]}]
  (loop [k 3]
    (when (and (<= k (+ distance 3)) (>= (- i k) 0))
      (if (<= (aget candle-consecutive-direction# (- i k)) (- count))
        (- i k)
        (recur (inc k))))))


(defn bull-expand-after [^long i ^long distance ^Quotes data]
  (let [^doubles open# (:open# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)]
    (loop [j 1]
      (when (and (< j distance) (>= (- i j 1) 0) (> (aget low# (- i j)) (aget open# i)))
        (if (and (== 1 (aget direction# (- i j)))
                 (== 1 (aget direction# (- i j 1)))
                 (> (aget close# (- i j 1))
                    (aget close# (- i j))
                    (aget close# i)))
          (- i j 1)
          (recur (inc j)))))))


(defn bull-expand-before [^long i ^long distance ^Quotes data]
  (let [^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        array-size (alength open#)]
    (loop [j 1]
      (when (and (< (+ i j 1) array-size) (<= j distance)
                 (< (aget high# (+ i j)) (aget close# i)))
        (if (and (== 1 (aget direction# (+ i j)))
                 (== 1 (aget direction# (+ i j 1)))
                 (< (aget close# (+ i j 1))
                    (aget close# (+ i j))
                    (aget close# i)))
          (+ i j 1)
          (recur (inc j)))))))


(defn bear-expand-after [^long i ^long distance ^Quotes data]
  (let [^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)]
    (loop [j 1]
      (when (and (< j distance) (>= (- i j 1) 0) (< (aget high# (- i j)) (aget open# i)))
        (if (and (== -1 (aget direction# (- i j)))
                 (== -1 (aget direction# (- i j 1)))
                 (< (aget close# (- i j 1))
                    (aget close# (- i j))
                    (aget close# i)))
          (- i j 1)
          (recur (inc j)))))))


(defn bear-expand-before [^long i ^long distance ^Quotes data]
  (let [^doubles open# (:open# data)
        ^doubles low# (:high# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        array-size (alength open#)]
    (loop [j 1]
      (when (and (< (+ i j 1) array-size) (<= j distance)
                 (> (aget low# (+ i j)) (aget close# i)))
        (if (and (== -1 (aget direction# (+ i j)))
                 (== -1 (aget direction# (+ i j 1)))
                 (> (aget close# (+ i j 1))
                    (aget close# (+ i j))
                    (aget close# i)))
          (+ i j 1)
          (recur (inc j)))))))


;(defmacro ^boolean bull-after-strong? [i ^doubles close# direction#]
;  `(and (> (- (alength (doubles close#)) i) 2)
;        (== 1 (aget direction# (- i 2)))
;        (== 1 (aget direction# (- i 1)))
;        (> (aget close# (- i 2))
;           (aget close# (- i 1))
;           (aget close# i))))
;
;
;(defmacro ^boolean bull-before-strong? [i ^doubles close#]
;  `(and (< (alength (doubles close#)) (+ i 2))
;        (< (aget close# (+ i 2))
;           (aget close# (+ i 1))
;           (aget close# `i))))
;
;
;(defmacro ^boolean bear-after-strong? [i ^doubles close# direction#]
;  `(and (> (- (alength (doubles close#)) i) 2)
;        (== -1 (aget direction# (- i 2)))
;        (== -1 (aget direction# (- i 1)))
;        (< (aget close# (- i 2))
;           (aget close# (- i 1))
;           (aget close# i))))
;
;
;(defmacro ^boolean bear-before-strong? [i ^doubles close#]
;  `(and (< (alength (doubles close#)) (+ i 2))
;        (> (aget close# (+ i 2))
;           (aget close# (+ i 1))
;           (aget close# i))))


;(defn ^boolean consecutive-bull-after? [i ^doubles open#
;                                            ^doubles low# ^doubles close#
;                                            ^longs direction# cnt
;                                            & {:keys [look-distance]
;                                               :or   {look-distance 64}}]
;  `(let [loop-limit# (min look-distance (- i cnt))
;         rng# (range (inc cnt))]
;     (loop [j# 1]
;       (if (< j# loop-limit#)
;         (and
;           (> (aget low# (- i j#)) (aget open# i))
;           (if (and (map #(== 1 %) (map #(aget direction# (- i j# %)) rng#))
;                    (apply > (map #(aget close# (- i j# %)) rng#)))
;             true
;             (recur (unchecked-inc-int j#))))
;         false))))
;
;
;(defmacro ^boolean consecutive-bear-after? [i ^doubles open#
;                                            ^doubles high# ^doubles close#
;                                            ^longs direction# cnt
;                                            & {:keys [look-distance]
;                                               :or   {look-distance 64}}]
;  `(let [loop-limit# (min look-distance (- i cnt))
;         rng# (range (inc cnt))]
;     (loop [j# 1]
;       (if (< j# loop-limit#)
;         (and
;           (< (aget high# (- i j#)) (aget open# i))
;           (if (and (map #(== -1 %) (map #(aget direction# (- i j# %)) rng#))
;                    (apply < (map #(aget close# (- i j# %)) rng#)))
;             true
;             (recur (unchecked-inc-int j#))))
;         false))))


;; (defmacro ^boolean consecutive? [cnt side dir i ^doubles open#
;;                                  ^doubles low# ^doubles close#
;;                                  ^longs direction#]
;;   `(let [array-size# (alength (doubles open#))
;;          loop-limit# (condp = side
;;                        :after (min look-distance i)
;;                        :before (min look-distance (- array-size# i)))
;;          fnc (condp = side
;;              :after -
;;              :before +)
;;          fcmp (condp = side
;;                 :after -
;;                 :before +)
;;          dir (condp = dir
;;                :bull 1
;;                :bear -1)]
;;      (loop [j# 1]
;;        (if (< j# loop-limit#)
;;          (and
;;           (> (aget low# (- i j#)) (aget open# i))
;;           (if (and (== 1 (aget direction# (- i j#)))
;;                    (== 1 (aget direction# (- i j# 1)))
;;                    (== 1 (aget direction# (- i j# 2)))
;;                    (> (aget close# (- i j# 2))
;;                       (aget close# (- i j# 1))
;;                       (aget close# (- i j#))
;;                       (aget close# i)))
;;             true
;;             (recur (unchecked-inc-int j#))))
;;          false))))
