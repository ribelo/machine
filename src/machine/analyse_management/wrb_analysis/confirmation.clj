(ns machine.analyse-management.wrb-analysis.confirmation
  (:require [machine.quotes.series :refer :all]
            [machine.analyse-management.wrb-analysis.utils :refer :all]
            [criterium.core :refer [bench quick-bench]])
  (:import [machine.quotes.core Quotes]))


(defn a
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^doubles midpoint# (:candle-body-midpoint# data)
        ^longs wrb# (:wrb-body# data)
        ^longs confirmation# (or (:confirmation data) (long-array array-size))
        ^longs confirmation-a# (long-array array-size)]
    (loop [i 2]
      (when (< i (- array-size 2))
        (cond
          ;; bull confirmation-a
          (and
            (== 1 (aget direction# i))
            (== -1 (aget wrb# (+ i 2)))
            (or (and (== -1 (aget direction# (inc i)))
                     (< (aget close# (inc i))
                        (aget close# (+ i 2))))
                (and (== 0 (aget direction# (inc i)))
                     (< (aget low# (inc i))
                        (aget close# (+ i 2)))))
            (> (aget close# i)
               (aget open# (inc i)))
            (> (aget close# i)
               (aget midpoint# (+ i 2))))
          (do
            (aset confirmation# i 1)
            (aset confirmation-a# i 1))
          ;; bear confirmation-a
          (and
            (== -1 (aget direction# i))
            (== 1 (aget wrb# (+ i 2)))
            (or (and (== 1 (aget direction# (inc i)))
                     (> (aget close# (inc i))
                        (aget close# (+ i 2))))
                (and (== 0 (aget direction# (inc i)))
                     (> (aget high# (inc i))
                        (aget close# (+ i 2)))))
            (< (aget close# i)
               (aget open# (inc i)))
            (< (aget close# i)
               (aget midpoint# (+ i 2))))
          (do
            (aset confirmation# i -1)
            (aset confirmation-a# i -1)))

        (recur (inc i))))
    (assoc data :confirmation confirmation#
                :confirmation-a confirmation-a#)))


(defn b
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^doubles midpoint# (:candle-body-midpoint# data)
        ^doubles body-size# (:candle-body-size# data)
        ^longs wrb# (:wrb-body# data)
        ^longs confirmation# (or (:confirmation data) (long-array array-size))
        ^longs confirmation-b# (long-array array-size)]
    (loop [i 2]
      (when (< i (- array-size 2))
        (cond
          ;; bull confirmation-b
          (and
            (== -1 (aget direction# (+ i 2)))
            (== -1 (aget wrb# (inc i)))
            (== 1 (aget direction# i))
            (< (aget low# i)
               (aget close# (inc i)))
            (< (aget open# i)
               (aget midpoint# (inc i)))
            (> (aget close# i)
               (aget midpoint# (inc i)))
            (> (aget body-size# i)
               (amax (aslice body-size# (+ i 2) (+ i 5)))))
          (do
            (aset confirmation# i 1)
            (aset confirmation-b# i 1))
          ;; bear confirmation-b
          (and
            (== 1 (aget direction# (+ i 2)))
            (== 1 (aget wrb# (inc i)))
            (== -1 (aget direction# i))
            (> (aget high# i)
               (aget close# (inc i)))
            (> (aget open# i)
               (aget midpoint# (inc i)))
            (< (aget close# i)
               (aget midpoint# (inc i)))
            (> (aget body-size# i)
               (amax (aslice body-size# (+ i 2) (+ i 5)))))
          (do
            (aset confirmation# i -1)
            (aset confirmation-b# i -1)))

        (recur (inc i))))
    (assoc data :confirmation confirmation#
                :confirmation-b confirmation-b#)))


(defn c
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^doubles midpoint# (:candle-body-midpoint# data)
        ^doubles body-size# (:candle-body-size# data)
        ^doubles upper-shadow# (:candle-upper-shadow# data)
        ^doubles bottom-shadow# (:candle-bottom-shadow# data)
        ^longs wrb# (:wrb-body# data)
        ^longs confirmation# (or (:confirmation data) (long-array array-size))
        ^longs confirmation-c# (long-array array-size)]
    (loop [i 5]
      (when (< i (- array-size 5))
        (cond
          ;; bull confirmation-c
          (and
            (== -1 (aget wrb# (+ i 2)))
            (== 1 (aget direction# (inc i)))
            (== 1 (aget direction# i))
            (> (aget close# i)
               (aget close# (inc i)))
            (>= (aget open# (inc i))
                (aget close# (+ i 2)))
            (< (aget low# (inc i))
               (aget close# (+ i 2)))
            (>= (aget low# i)
                (aget close# (+ i 2)))
            (< (aget close# i)
               (aget open# (+ i 2)))
            (<= (aget high# i)
                (aget open# (+ i 2)))
            (> (aget high# i)
               (aget midpoint# (+ i 2)))
            (or (< (aget body-size# (inc i))
                   (amin (aslice body-size# (+ i 3) (+ i 6))))
                (> (aget bottom-shadow# (inc i))
                   (amax (aslice bottom-shadow# (+ i 3) (+ i 6)))))
            (> (amin (aslice high# (+ i 3) (+ i 6)))
               (aget open# (+ i 2))))
          (do
            (aset confirmation# i 1)
            (aset confirmation-c# i 1))
          ;; bear confirmation-c
          (and
            (== 1 (aget wrb# (+ i 2)))
            (== -1 (aget direction# (inc i)))
            (== -1 (aget direction# i))
            (< (aget close# i)
               (aget close# (inc i)))
            (<= (aget open# (inc i))
                (aget close# (+ i 2)))
            (> (aget high# (inc i))
               (aget close# (+ i 2)))
            (<= (aget high# i)
                (aget close# (+ i 2)))
            (> (aget close# i)
               (aget open# (+ i 2)))
            (>= (aget low# i)
                (aget open# (+ i 2)))
            (< (aget low# i)
               (aget midpoint# (+ i 2)))
            (or (< (aget body-size# (inc i))
                   (amin (aslice body-size# (+ i 3) (+ i 6))))
                (> (aget upper-shadow# (inc i))
                   (amax (aslice upper-shadow# (+ i 3) (+ i 6)))))
            (< (amin (aslice low# (+ i 3) (+ i 6)))
               (aget open# (+ i 2))))
          (do
            (aset confirmation# i -1)
            (aset confirmation-c# i -1)))

        (recur (inc i))))
    (assoc data :confirmation confirmation#
                :confirmation-c confirmation-c#)))


(defn d
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs confirmation# (or (:confirmation data) (long-array array-size))
        ^longs confirmation-d# (long-array array-size)]
    (loop [i 2]
      (when (< i (- array-size 2))
        (cond
          ;; bull confirmation-d
          (and
            (== -1 (aget direction# (+ i 2)))
            (not= 0 (aget direction# (int i)))
            (== 1 (aget direction# i))
            (or
              ;; d1
              (and (< (aget open# i)
                      (aget close# (inc i)))
                   (> (aget close# i)
                      (aget open# (inc i)))
                   (< (aget open# (inc i))
                      (aget low# (+ i 2))))
              ;; d2
              (and (< (aget open# i)
                      (aget close# (+ i 2)))
                   (> (aget close# i)
                      (aget open# (+ i 2)))
                   (< (aget high# (inc i))
                      (aget open# (+ i 2)))
                   (< (Math/max ^double (aget open# (inc i))
                                ^double (aget close# (inc i)))
                      (aget low# (+ i 2))))))
          (do
            (aset confirmation# i 1)
            (aset confirmation-d# i 1))
          ;; bear confirmation-d
          (and
            (== 1 (aget direction# (+ i 2)))
            (not= 0 (aget direction# (int i)))
            (== -1 (aget direction# i))
            (or
              ;; d1
              (and (> (aget open# i)
                      (aget close# (inc i)))
                   (< (aget close# i)
                      (aget open# (inc i)))
                   (> (aget open# (inc i))
                      (aget high# (+ i 2))))
              ;; d2
              (and (> (aget open# i)
                      (aget close# (+ i 2)))
                   (< (aget close# i)
                      (aget open# (+ i 2)))
                   (> (aget low# (inc i))
                      (aget open# (+ i 2)))
                   (> (Math/min ^double (aget open# (inc i))
                                ^double (aget close# (inc i)))
                      (aget high# (+ i 2))))))
          (do
            (aset confirmation# i -1)
            (aset confirmation-d# i -1)))

        (recur (inc i))))
    (assoc data :confirmation confirmation#
                :confirmation-d confirmation-d#)))


(defn e
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^doubles midpoint# (:candle-body-midpoint# data)
        ^longs confirmation# (or (:confirmation data) (long-array array-size))
        ^longs confirmation-e# (long-array array-size)]
    (loop [i 3]
      (when (< i (- array-size 3))
        (cond
          ;; bull confirmation-e
          (and
            (== -1 (aget direction# (+ i 3)))
            (== -1 (aget direction# (+ i 2)))
            (== 1 (aget direction# (inc i)))
            (== 1 (aget direction# i))
            (> (aget open# (inc i))
               (aget close# (+ i 2)))
            (> (aget close# i)
               (aget close# (inc i)))
            (> (aget close# i)
               (aget open# (+ i 2)))
            (< (aget close# (+ i 2))
               (aget close# (+ i 3)))
            (< (aget close# i)
               (aget midpoint# (+ i 3))))
          (do
            (aset confirmation# i 1)
            (aset confirmation-e# i 1))
          ;; bear confirmation-e
          (and
            (== 1 (aget direction# (+ i 3)))
            (== 1 (aget direction# (+ i 2)))
            (== -1 (aget direction# (inc i)))
            (== -1 (aget direction# i))
            (< (aget open# (inc i))
               (aget close# (+ i 2)))
            (< (aget close# i)
               (aget close# (inc i)))
            (< (aget close# i)
               (aget open# (+ i 2)))
            (> (aget close# (+ i 2))
               (aget close# (+ i 3)))
            (> (aget close# i)
               (aget midpoint# (+ i 3))))
          (do
            (aset confirmation# i -1)
            (aset confirmation-e# i -1)))

        (recur (inc i))))
    (assoc data :confirmation confirmation#
                :confirmation-e confirmation-e#)))

(defn f
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs wrb# (:wrb-body# data)
        ^longs confirmation# (or (:confirmation data) (long-array array-size))
        ^longs confirmation-f# (long-array array-size)]
    (loop [i 3]
      (when (< i (- array-size 3))
        (cond
          ;; bull confirmation-f#
          (and (== 1 (aget wrb# i))
               (loop [j 4]
                 (when (< j (Math/min 8 (- array-size i)))
                   (cond (some #{-1 1} (aslice wrb# (inc i) (inc (+ i j))))
                         false

                         (and
                           (== -1 (aget direction# (+ i j)))
                           (> (aget close# i)
                              (aget open# (+ i j)))
                           (<= (aget close# i)
                               (aget high# (+ i j)))
                           (< (Math/min ^double (aget low# (+ i j))
                                        ^double (aget low# (dec (+ i j))))
                              (Math/min ^double (amin (aslice open# (inc i) (- (+ i j) 2)))
                                        ^double (amin (aslice close# (inc i) (- (+ i j) 2)))))
                           (>= (aget open# (+ i j))
                               (Math/max ^double (amax (aslice open# (inc i) (- (+ i j) 2)))
                                         ^double (amax (aslice close# (inc i) (- (+ i j) 2)))))
                           (> (aget close# (+ i j))
                              (Math/max ^double (amax (aslice open# (inc i) (- (+ i j) 2)))
                                        ^double (amax (aslice close# (inc i) (- (+ i j) 2))))))
                         true
                         :else (recur (inc j))))))
          (do
            (aset confirmation# i 1)
            (aset confirmation-f# i 1))
          ;; bear confirmation-f#
          (and (== -1 (aget wrb# i))
               (loop [j 4]
                 (when (< j (Math/min 8 (- array-size i)))
                   (cond (some #{-1 1} (aslice wrb# (inc i) (inc (+ i j))))
                         false

                         (and
                           (== 1 (aget direction# (+ i j)))
                           (< (aget close# i)
                              (aget open# (+ i j)))
                           (>= (aget close# i)
                               (aget low# (+ i j)))
                           (> (Math/max ^double (aget high# (+ i j))
                                        ^double (aget high# (dec (+ i j))))
                              (Math/max ^double (amax (aslice open# (inc i) (- (+ i j) 2)))
                                        ^double (amax (aslice close# (inc i) (- (+ i j) 2)))))
                           (<= (aget open# (+ i j))
                               (Math/min ^double (amin (aslice open# (inc i) (- (+ i j) 2)))
                                         ^double (amin (aslice close# (inc i) (- (+ i j) 2)))))
                           (< (aget close# (+ i j))
                              (Math/min ^double (amin (aslice open# (inc i) (- (+ i j) 2)))
                                        ^double (amin (aslice close# (inc i) (- (+ i j) 2))))))
                         true
                         :else (recur (inc j))))))
          (do
            (aset confirmation# i -1)
            (aset confirmation-f# i -1)))

        (recur (inc i))))
    (assoc data :confirmation confirmation#
                :confirmation-f confirmation-f#)))



(defn g
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs wrb# (:wrb-body# data)
        ^longs confirmation# (or (:confirmation data) (long-array array-size))
        ^longs confirmation-g# (long-array array-size)]
    (loop [i 3]
      (when (< i (- array-size 3))
        (cond
          ;; bull confirmation-f#
          (and (== 1 (aget wrb# i))
               (loop [j 4]
                 (when (< j (Math/min 8 (- array-size i)))
                   (cond (some #{-1 1} (aslice wrb# (inc i) (inc (+ i j))))
                         false

                         (and
                           (== 1 (aget direction# (+ i j)))
                           (> (aget close# i)
                              (aget close# (+ i j)))
                           (<= (aget close# i)
                               (aget high# (+ i j)))
                           (< (Math/min ^double (aget low# (+ i j))
                                        ^double (aget low# (dec (+ i j))))
                              (Math/min ^double (amin (aslice open# (inc i) (- (+ i j) 2)))
                                        ^double (amin (aslice close# (inc i) (- (+ i j) 2)))))
                           (>= (aget high# (+ i j))
                               (Math/max ^double (amax (aslice open# (inc i) (- (+ i j) 2)))
                                         ^double (amax (aslice close# (inc i) (- (+ i j) 2)))))
                           (> (aget close# (+ i j))
                              (Math/max ^double (amax (aslice open# (inc i) (- (+ i j) 2)))
                                        ^double (amax (aslice close# (inc i) (- (+ i j) 2))))))
                         true
                         :else (recur (inc j))))))
          (do
            (aset confirmation# i 1)
            (aset confirmation-g# i 1))
          ;; bear confirmation-g#
          (and (aget wrb# i)
               (== -1 (aget direction# i))
               (loop [j 4]
                 (when (< j (Math/min 8 (- array-size i)))
                   (cond (some #{-1 1} (aslice wrb# (inc i) (inc (+ i j))))
                         false

                         (and
                           (== -1 (aget direction# (+ i j)))
                           (< (aget close# i)
                              (aget close# (+ i j)))
                           (>= (aget close# i)
                               (aget low# (+ i j)))
                           (> (Math/max ^double (aget high# (+ i j))
                                        ^double (aget high# (dec (+ i j))))
                              (Math/max ^double (amax (aslice open# (inc i) (- (+ i j) 2)))
                                        ^double (amax (aslice close# (inc i) (- (+ i j) 2)))))
                           (<= (aget low# (+ i j))
                               (Math/min ^double (amin (aslice open# (inc i) (- (+ i j) 2)))
                                         ^double (amin (aslice close# (inc i) (- (+ i j) 2)))))
                           (< (aget close# (+ i j))
                              (Math/min ^double (amin (aslice open# (inc i) (- (+ i j) 2)))
                                        ^double (amin (aslice close# (inc i) (- (+ i j) 2))))))
                         true
                         :else (recur (inc j))))))
          (do
            (aset confirmation# i -1)
            (aset confirmation-g# i -1)))

        (recur (inc i))))
    (assoc data :confirmation confirmation#
                :confirmation-g confirmation-g#)))


(defn h1
  ^Quotes [^Quotes data ^long contraction-size]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs filled-by# (:candle-filled-by# data)
        ^longs wrb# (:wrb-body# data)
        ^longs hg# (:wrb-hg-body# data)
        ^longs confirmation-h1# (long-array array-size)
        ^longs confirmation-h1-contraction# (long-array array-size)
        ^doubles zone-open# (double-array array-size)
        ^doubles zone-close# (double-array array-size)
        ^longs zone-contraction-count# (long-array array-size)
        ^longs zone-confirmation-candle# (long-array array-size)
        ^doubles zone-size# (double-array array-size)
        ^longs zone-filled-by# (long-array array-size)
        ^doubles zone-zero-line# (double-array array-size)]
    (loop [i 0]
      (when (< i (- array-size 4))
        (loop [j 4]
          (when (< j (Math/min ^long (+ contraction-size 4)
                               ^long (- array-size i)))
            ;; bull confirmation-h1
            (if
              (or
                (and
                  (== 1 (aget hg# i))
                  (== 1 (aget wrb# (+ i j)))
                  (contraction-share? i (+ i j) data)
                  (contraction-break-bull? i (+ i j) data)
                  ;(expand/bear-before-weak? (+ i j) data)
                  )
                ;; bear confirmation-h1
                (and
                  (== -1 (aget hg# i))
                  (== -1 (aget wrb# (+ i j)))
                  (contraction-share? i (+ i j) data)
                  (contraction-break-bear? i (+ i j) data)
                  ;(expand/bull-before-weak? (+ i j) data)
                  ))
              (do
                (aset confirmation-h1# i (aget direction# i))
                (aset confirmation-h1-contraction# i j)
                (aset zone-open# i (aget open# i))
                (aset zone-close# i (aget close# i))
                (aset zone-contraction-count# i j)
                (aset zone-confirmation-candle# i (dec i))
                (aset zone-size# i (Math/abs (- (aget open# i) (aget close# i))))
                (aset zone-filled-by# i (aget filled-by# i))
                (aset zone-zero-line# i (condp = (aget direction# i)
                                          1 (amax (aslice high# (inc i) (+ i j)))
                                          -1 (amin (aslice low# (inc i) (+ i j))))))
              (recur (inc j)))))
        (recur (inc i))))
    (assoc data :confirmation-h1 confirmation-h1#
                :confirmation-h1-contraction confirmation-h1-contraction#
                :zone-h1 confirmation-h1#
                :zone-h1-contraction confirmation-h1-contraction#
                :zone-h1-confirmation-nr zone-confirmation-candle#
                :zone-h1-open zone-open#
                :zone-h1-close zone-close#
                :zone-h1-size zone-size#
                :zone-h1-filled-by zone-filled-by#
                :zone-h1-zero-line zone-zero-line#)))


(defn h2
  ^Quotes [^Quotes data ^long contraction-size]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs filled-by# (:candle-filled-by# data)
        ^longs wrb# (:wrb-body# data)
        ^longs hg# (:wrb-hg-body# data)
        ^longs confirmation-h2# (long-array array-size)
        ^longs confirmation-h2-contraction# (long-array array-size)
        ^doubles zone-open# (double-array array-size)
        ^doubles zone-close# (double-array array-size)
        ^longs zone-contraction-count# (long-array array-size)
        ^longs zone-confirmation-candle# (long-array array-size)
        ^doubles zone-size# (double-array array-size)
        ^longs zone-filled-by# (long-array array-size)
        ^doubles zone-zero-line# (double-array array-size)]
    (loop [i 0]
      (when (< i (- array-size 4))
        (loop [j 4]
          (when (< j (Math/min ^long (+ contraction-size 4)
                               ^long (- array-size i)))
            (if
              (or
                ;; bull confirmation-h2
                (and
                  (== 1 (aget hg# i))
                  (== -1 (aget wrb# (+ i j)))
                  (contraction-share? i (+ i j) data)
                  (contraction-break-bull? i (+ i j) data)
                  ;(expand/bear-before-weak? (+ i j) data)
                  )
                ;; bear confirmation-h2
                (and
                  (== -1 (aget hg# i))
                  (== 1 (aget wrb# (+ i j)))
                  (contraction-share? i (+ i j) data)
                  (contraction-break-bear? i (+ i j) data)
                  ;(expand/bull-before-weak? (+ i j) data)
                  ))
              (do
                (aset confirmation-h2# i (aget direction# i))
                (aset confirmation-h2-contraction# i j)
                (aset zone-open# i (aget open# i))
                (aset zone-close# i (aget close# i))
                (aset zone-contraction-count# i j)
                (aset zone-confirmation-candle# i (dec i))
                (aset zone-size# i (Math/abs (- (aget open# i) (aget close# i))))
                (aset zone-filled-by# i (aget filled-by# i))
                (aset zone-zero-line# i (condp = (aget direction# i)
                                          1 (amax (aslice high# (inc i) (+ i j)))
                                          -1 (amin (aslice low# (inc i) (+ i j))))))

              (recur (inc j)))))

        (recur (inc i))))
    (assoc data :confirmation-h2# confirmation-h2#
                :confirmation-h2-contraction# confirmation-h2-contraction#
                :zone-h2# confirmation-h2#
                :zone-h2-contraction# confirmation-h2-contraction#
                :zone-h2-confirmation-nr# zone-confirmation-candle#
                :zone-h2-open# zone-open#
                :zone-h2-close# zone-close#
                :zone-h2-size# zone-size#
                :zone-h2-filled-by# zone-filled-by#
                :zone-h2-zero-line# zone-zero-line#)))


(defn h3
  ^Quotes [^Quotes data ^long contraction-size]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs filled-by# (:candle-filled-by# data)
        ^longs wrb# (:wrb-body# data)
        ^longs hg# (:wrb-hg-body# data)
        ^longs confirmation-h3# (long-array array-size)
        ^longs confirmation-h3-contraction# (long-array array-size)
        ^doubles zone-open# (double-array array-size)
        ^doubles zone-close# (double-array array-size)
        ^longs zone-contraction-count# (long-array array-size)
        ^longs zone-confirmation-candle# (long-array array-size)
        ^doubles zone-size# (double-array array-size)
        ^longs zone-filled-by# (long-array array-size)
        ^doubles zone-zero-line# (double-array array-size)]
    (loop [i 0]
      (when (< i (- array-size 4))
        (loop [j 4]
          (when (< j (Math/min ^long (+ contraction-size 4)
                               ^long (- array-size i)))
            (if
              (or
                ;; bull confirmation-h3
                (and
                  (== 1 (aget hg# i))
                  (== 1 (aget wrb# (+ i j)))
                  (contraction-share? i (+ i j) data)
                  (contraction-size-break? i (+ i j) data)
                  ;(expand/bear-before-weak? (+ i j) data)
                  )
                ;; bear confirmation-h3
                (and
                  (== -1 (aget hg# i))
                  (== -1 (aget wrb# (+ i j)))
                  (contraction-share? i (+ i j) data)
                  (contraction-size-break? i (+ i j) data)
                  ;(expand/bull-before-weak? (+ i j) data)
                  ))
              (do
                (aset confirmation-h3# i (aget direction# i))
                (aset confirmation-h3-contraction# i j)
                (aset zone-open# i (aget open# i))
                (aset zone-close# i (aget close# i))
                (aset zone-contraction-count# i j)
                (aset zone-confirmation-candle# i (dec i))
                (aset zone-size# i (Math/abs (- (aget open# i) (aget close# i))))
                (aset zone-filled-by# i (aget filled-by# i))
                (aset zone-zero-line# i (condp = (aget direction# i)
                                          1 (amax (aslice high# (inc i) (+ i j)))
                                          -1 (amin (aslice low# (inc i) (+ i j))))))
              (recur (inc j)))))
        (recur (inc i))))
    (assoc data :confirmation-h3 confirmation-h3#
                :confirmation-h3-contraction confirmation-h3-contraction#
                :zone-h3 confirmation-h3#
                :zone-h3-contraction confirmation-h3-contraction#
                :zone-h3-confirmation-nr zone-confirmation-candle#
                :zone-h3-open zone-open#
                :zone-h3-close zone-close#
                :zone-h3-size zone-size#
                :zone-h3-filled-by zone-filled-by#
                :zone-h3-zero-line zone-zero-line#)))


(defn h4
  ^Quotes [^Quotes data ^long contraction-size]
  (let [array-size (alength ^objects (:time# data))
        ^doubles open# (:open# data)
        ^doubles high# (:high# data)
        ^doubles low# (:low# data)
        ^doubles close# (:close# data)
        ^longs direction# (:candle-direction# data)
        ^longs filled-by# (:candle-filled-by# data)
        ^longs wrb# (:wrb-body# data)
        ^longs confirmation-h4# (long-array array-size)
        ^longs confirmation-h4-contraction# (long-array array-size)
        ^doubles zone-open# (double-array array-size)
        ^doubles zone-close# (double-array array-size)
        ^longs zone-contraction-count# (long-array array-size)
        ^longs zone-confirmation-candle# (long-array array-size)
        ^doubles zone-size# (double-array array-size)
        ^longs zone-filled-by# (long-array array-size)
        ^doubles zone-zero-line# (double-array array-size)]
    (loop [i 0]
      (when (< i (- array-size 4))
        (loop [j 4]
          (when (< j (Math/min ^long (+ contraction-size 4)
                               ^long (- array-size i)))
            (if
              (or
                ;; bull confirmation-h4
                (and
                  (== 1 (aget wrb# i))
                  (== -1 (aget wrb# (+ i j)))
                  (contraction-share? i (+ i j) data)
                  (contraction-break-bull? i (+ i j) data)
                  ;(expand/bear-before-weak? (+ i j) data)
                  )
                ;; bear confirmation-h4
                (and
                  (== -1 (aget wrb# i))
                  (== 1 (aget wrb# (+ i j)))
                  (contraction-share? i (+ i j) data)
                  (contraction-break-bear? i (+ i j) data)
                  ;(expand/bull-before-weak? (+ i j) data)
                  ))
              (do
                (aset confirmation-h4# i (aget direction# i))
                (aset confirmation-h4-contraction# i j)
                (aset zone-open# i
                      (aget open# i))
                (aset zone-close# i
                      (aget close# i))
                (aset zone-contraction-count# i j)
                (aset zone-confirmation-candle# i (dec i))
                (aset zone-size# i (Math/abs (- (aget open# i) (aget close# i))))
                (aset zone-filled-by# i (aget filled-by# i))
                (aset zone-zero-line# i (condp = (aget direction# i)
                                          1 (amax (aslice high# (inc i) (+ i j)))
                                          -1 (amin (aslice low# (inc i) (+ i j))))))
              (recur (inc j)))))
        (recur (inc i))))
    (assoc data :confirmation-h4 confirmation-h4#
                :confirmation-h4-contraction confirmation-h4-contraction#
                :zone-h4 confirmation-h4#
                :zone-h4-contraction confirmation-h4-contraction#
                :zone-h4-confirmation-nr zone-confirmation-candle#
                :zone-h4-open zone-open#
                :zone-h4-close zone-close#
                :zone-h4-size zone-size#
                :zone-h4-filled-by zone-filled-by#
                :zone-h4-zero-line zone-zero-line#)))

(defn analyse
  ^Quotes [^Quotes data contraction-size]
  (-> data
      ;(a)
      ;(b)
      ;(c)
      ;(d)
      ;(e)
      ;(f)
      ;(g)
      (h1 contraction-size)
      (h2 contraction-size)
      (h3 contraction-size)
      (h4 contraction-size)
      ))