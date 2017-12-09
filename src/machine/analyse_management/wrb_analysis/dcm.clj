(ns machine.analyse-management.wrb-analysis.dcm
  (:require [machine.quotes.series :refer :all]
            [machine.analyse-management.wrb-analysis.utils :refer :all])
  (:import [org.apache.commons.math3.util FastMath]
           [machine.quotes.core Quotes]))


(defn- dcm-conntraction-bull
  ([data most-recent-bear-hg first-bull-hg second-bull-hg]
   (let [^doubles open# (:open# data)
         ^doubles close# (:close# data)
         ^doubles body-size# (:candle-body-size# data)
         ^longs filled-by# (:candle-filled-by# data)
         loop-limit (- second-bull-hg 3)]
     (loop [j 1]
       (if (< j loop-limit)
         (cond
           (or (> (aget filled-by# first-bull-hg) (- second-bull-hg j 3))
               (> (aget filled-by# second-bull-hg) (- second-bull-hg j 3)))
           false

           (and (> (amin (aslice close# (- second-bull-hg j 4) (- second-bull-hg (inc j))))
                   (aget open# most-recent-bear-hg))
                (> (amax (aslice close# (- second-bull-hg j 4) (- second-bull-hg (inc j))))
                   (FastMath/max ^double (aget close# first-bull-hg) ^double (aget close# second-bull-hg)))
                (< (amax (aslice body-size# (- second-bull-hg j 4) (- second-bull-hg (inc j))))
                   (FastMath/min ^double (aget body-size# first-bull-hg)
                                 ^double (aget body-size# second-bull-hg))))
           (FastMath/max 0.0 ^double (- second-bull-hg j 3))

           :else (recur (inc j)))
         false))))
  ([data most-recent-bear-hg first-bull-hg]
   (let [^doubles open# (:open# data)
         ^doubles close# (:close# data)
         ^doubles body-size# (:candle-body-size# data)
         ^longs filled-by# (:candle-filled-by# data)
         loop-limit (- first-bull-hg 3)]
     (loop [j 1]
       (if (< j loop-limit)
         (cond
           (> (aget filled-by# first-bull-hg) (- first-bull-hg j 3))
           false

           (and (> (amin (aslice close# (- first-bull-hg j 4) (- first-bull-hg (inc j))))
                   (aget open# most-recent-bear-hg))
                (> (amax (aslice close# (- first-bull-hg j 4) (- first-bull-hg (inc j))))
                   (aget close# first-bull-hg))
                (< (amax (aslice body-size# (- first-bull-hg j 4) (- first-bull-hg (inc j))))
                   (aget body-size# first-bull-hg)))
           (FastMath/max 0.0 ^double (- first-bull-hg j 3))

           :else (recur (inc j)))
         false)))))


(defn- dcm-conntraction-bear
  ([data most-recent-bull-hg first-bear-hg second-bear-hg]
   (let [^doubles open# (:open# data)
         ^doubles close# (:close# data)
         ^doubles body-size# (:candle-body-size# data)
         ^longs filled-by# (:candle-filled-by# data)
         loop-limit (- second-bear-hg 3)]
     (loop [j 1]
       (if (< j loop-limit)
         (cond
           (or (> (aget filled-by# first-bear-hg) (- first-bear-hg j 3))
               (> (aget filled-by# second-bear-hg) (- second-bear-hg j 3)))
           false

           (and (< (amax (aslice close# (- second-bear-hg j 4) (- second-bear-hg (inc j))))
                   (aget open# most-recent-bull-hg))
                (< (amax (aslice close# (- second-bear-hg j 4) (- second-bear-hg (inc j))))
                   (FastMath/max ^double (aget close# first-bear-hg) ^double (aget close# second-bear-hg)))
                (< (amax (aslice body-size# (- second-bear-hg j 4) (- second-bear-hg (inc j))))
                   (FastMath/min ^double (aget body-size# first-bear-hg)
                                 ^double (aget body-size# second-bear-hg))))
           (FastMath/max 0 (- second-bear-hg j 3))

           :else (recur (inc j)))
         false))))
  ([data most-recent-bull-hg first-bear-hg]
   (let [^doubles open# (:open# data)
         ^doubles close# (:close# data)
         ^doubles body-size# (:candle-body-size# data)
         ^longs filled-by# (:candle-filled-by# data)
         loop-limit (- first-bear-hg 3)]
     (loop [j 1]
       (if (< j loop-limit)
         (cond
           (> (aget filled-by# first-bear-hg) (- first-bear-hg j 3))
           false

           (and (< (amax (aslice close# (- first-bear-hg j 4) (- first-bear-hg (inc j))))
                   (aget open# most-recent-bull-hg))
                (< (amax (aslice close# (- first-bear-hg j 4) (- first-bear-hg (inc j))))
                   (aget close# first-bear-hg))
                (< (amax (aslice body-size# (- first-bear-hg j 4) (- first-bear-hg (inc j))))
                   (aget body-size# first-bear-hg)))
           (FastMath/max 0 ^long (- first-bear-hg j 3))

           :else (recur (inc j)))
         false)))))



(defn dcm
  ([^Quotes data]
   (dcm data nil))
  ([^Quotes data zone-types]
   (let [array-size (alength ^objects (:time# data))
         ^longs hg-body# (:wrb-hg-body# data)
         zone# (when zone-types (identity-union (mapv #(get data (zone-keyword %)) zone-types)))
         ^longs dcm-direction# (long-array array-size)
         ^longs dcm-pullback# (long-array array-size)
         ^longs dcm-trend# (long-array array-size)]
     (loop [i (- array-size 3) last-dcm 0]
       (when (>= i 0)
         (when zone#
           (condp == (aget ^longs zone# i)
             1 (aset dcm-direction# i -1)
             -1 (aset dcm-direction# i 1)
             nil))
         (condp == (aget hg-body# i)
           -1 (loop [j 3 first-hg nil second-hg nil]
                (cond
                  (and first-hg second-hg)
                  (when-let [ccv (dcm-conntraction-bull data i first-hg second-hg)]
                    (aset dcm-direction# (long ccv) 1))
                  (and first-hg (== 1 last-dcm))
                  (when-let [ccv (dcm-conntraction-bull data i first-hg)]
                    (aset dcm-direction# (long ccv) 1)
                    (aset dcm-pullback# i -1))
                  :else (when (<= j i)
                          (if (== 1 (aget hg-body# (- i j)))
                            (cond
                              (not first-hg) (recur (inc j) (- i j) nil)
                              (not second-hg) (recur (inc j) first-hg (- i j)))
                            (recur (inc j) first-hg second-hg)))))
           1 (loop [j 3 first-hg nil second-hg nil]
               (cond
                 (and first-hg second-hg)
                 (when-let [ccv (dcm-conntraction-bear data i first-hg second-hg)]
                   (aset dcm-direction# (long ccv) -1)
                   (aset dcm-pullback# i 1))
                 (and first-hg (== -1 last-dcm))
                 (when-let [ccv (dcm-conntraction-bear data i first-hg)]
                   (aset dcm-direction# (long ccv) -1))
                 :else (when (<= j i)
                         (if (== -1 (aget hg-body# (- i j)))
                           (cond
                             (not first-hg) (recur (inc j) (- i j) nil)
                             (not second-hg) (recur (inc j) first-hg (- i j)))
                           (recur (inc j) first-hg second-hg)))))
           nil)

         (condp == (aget dcm-direction# i)
           -1 (aset dcm-trend# i -1)
           1 (aset dcm-trend# i 1)
           (aset dcm-trend# i (aget dcm-trend# (inc i))))

         (recur (dec i) (aget dcm-trend# (inc i)))))
     (assoc data :wrb-dcm-direction# dcm-direction#
                 :wrb-dcm-trend# dcm-trend#
                 :wrb-dcm-pullback# dcm-pullback#))))


;(defn dcm [data & {:keys [look-distance] :or {look-distance 64}}]
;  (let [array-size (alength ^objects (:time data))
;        ^longs strong-continuation# (:strong-continuation data)
;        ^longs dcm# (long-array array-size)]
;    (loop [i (- array-size 3)]
;      (when (and (>= i 0))
;        (condp = (aget strong-continuation# i)
;          1 (aset dcm# i 1)
;          -1 (aset dcm# i -1)
;          0 (aset dcm# i (aget dcm# (inc i))))
;        (recur (dec i))))
;    (assoc data :wrb-dcm dcm#)))


;(defn dcm [data & {:keys [look-distance] :or {look-distance 64}}]
;  (let [array-size (alength ^objects (:time data))
;        ^doubles close# (:close# data)
;        ^doubles ma# (sma close# 200)
;        ^longs dcm# (long-array array-size)]
;    (loop [i (- array-size 3)]
;      (when (and (>= i 0))
;        (if (> (aget close# i) (aget ma# i))
;          (aset dcm# i 1)
;          (aset dcm# i -1))
;        (recur (dec i))))
;    (assoc data :wrb-dcm dcm#)))

