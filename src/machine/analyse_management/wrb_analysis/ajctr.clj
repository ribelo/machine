(ns machine.analyse-management.wrb-analysis.ajctr
  (:require [machine.quotes.series :refer :all]
            [machine.analyse-management.wrb-analysis.utils :refer :all]
            [criterium.core :refer [bench quick-bench]])
  (:import [machine.quotes.core Quotes]))


(set! *unchecked-math* true)


(definline big-body-bull?
  [i ^doubles body-size# ^longs wrb#]
  `(let [loop-limit# (- (alength ~body-size#) ~i)]
     (loop [j# 1 cnt# 0]
       (when (< j# loop-limit#)
         (cond (>= cnt# 3)
               true
               (<= (aget ~body-size# ~i)
                   (aget ~body-size# (+ ~i j#)))

               false
               (== -1 (aget ~wrb# (+ ~i j#)))
               (recur (inc j#) 1)

               :else (recur (inc j#) (inc cnt#)))))))


(definline big-body-bear?
  [i ^doubles body-size# ^longs wrb#]
  `(let [loop-limit# (- (alength ~body-size#) ~i)]
     (loop [j# 1 cnt# 0]
       (when (< j# loop-limit#)
         (cond (>= cnt# 3)
               true

               (<= (aget ~body-size# ~i)
                   (aget ~body-size# (+ ~i j#)))
               false

               (== 1 (aget ~wrb# (+ ~i j#)))
               (recur (inc j#) 1)

               :else (recur (inc j#) (inc cnt#)))))))


(defn deep-shadow-upper?
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time data))
        loop-limit (- array-size 3)
        ^doubles body-size# (:candle-body-size# data)
        ^doubles upper-shadow# (:candle-upper-shadow# data)
        ^longs wrb# (:wrb-body# data)
        ^booleans result# (boolean-array array-size)]
    (loop [i 0]
      (when (< i loop-limit)
        (loop [j 1 cnt 0]
          (when (< j (- array-size i))
            (cond
              (>= cnt 3)
              (aset result# i true)

              (or (<= (aget upper-shadow# i)
                      (aget upper-shadow# (+ i j)))
                  (<= (aget upper-shadow# i)
                      (aget body-size# (+ i j))))
              (aset result# i false)

              (== 1 (aget wrb# (+ i j)))
              (recur (inc j) 1)

              :else (recur (inc j) (inc cnt)))))
        (recur (inc i))))
    (assoc data :ajctr-deep-shadow-upper? result#)))


(defn deep-shadow-bottom?
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time data))
        loop-limit (- array-size 3)
        ^doubles body-size# (:candle-body-size# data)
        ^doubles bottom-shadow# (:candle-bottom-shadow# data)
        ^longs wrb# (:wrb-body# data)
        ^booleans result# (boolean-array array-size)]
    (loop [i 0]
      (when (< i loop-limit)
        (loop [j 1 cnt 0]
          (when (< j (- array-size i))
            (cond
              (>= cnt 3)
              (aset result# i true)

              (or (<= (aget bottom-shadow# i)
                      (aget bottom-shadow# (+ i j)))
                  (<= (aget bottom-shadow# i)
                      (aget body-size# (+ i j))))
              (aset result# i false)

              (== -1 (aget wrb# (+ i j)))
              (recur (inc j) 1)

              :else (recur (inc j) (inc cnt)))))
        (recur (inc i))))
    (assoc data :ajctr-deep-shadow-bottom? result#)))


(defn hammer
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time data))
        loop-limit (- array-size 3)
        ^doubles open# (:open data)
        ^doubles high# (:high data)
        ^doubles low# (:low data)
        ^doubles close# (:close data)
        ^longs direction# (:candle-direction# data)
        ^doubles body-size# (:candle-body-size# data)
        ^doubles upper-shadow# (:candle-upper-shadow# data)
        ^doubles bottom-shadow# (:candle-bottom-shadow# data)
        ^booleans deep-upper-shadow# (:ajctr-deep-shadow-upper? data)
        ^booleans deep-bottom-shadow# (:ajctr-deep-shadow-bottom? data)
        ^longs hammer# (long-array array-size)]
    (loop [i 0]
      (when (< i loop-limit)
        (cond
          ;; bull hammer
          (and
            ;(== 1 (aget zone-inside# i))
            ;; Body is white (Close > Open)
            (== 1 (aget direction# i))
            ;; Body (Close - Open) is greater than Upper Shadow (High - Close)
            (> (aget body-size# i)
               (aget upper-shadow# i))
            ;; Lower shadow (Open - Low) is greater than the combination of the Body and Upper Shadow
            (> (aget bottom-shadow# i)
               (+ (aget body-size# i)
                  (aget upper-shadow# i)))
            ;; Long Lower Shadow of the White Hammer Line (c1) must have more depth (longer)
            ;; than the lower shadows of the prior three intervals.
            (aget deep-bottom-shadow# i)
            ;; Low of the White Hammer Line < Lows of the prior three intervals.
            (< (aget low# i)
               (amin (aslice low# (+ i 1) (+ i 4))))
            ;; There must be a Dark Line with its Low > Open of the White Hammer Line.
            (> (amax (aslice low# (+ i 1) (+ i 4)))
               (aget open# i))
            ;; Close of White Hammer Line < or = Highest High among the prior two candlestick lines.
            (<= (aget close# i)
                (amax (aslice high# (+ i 1) (+ i 3))))
            ;; If the interval before the White Hammer Line is a white candlestick (Close > Open)...
            (if (== 1 (aget direction# (inc i)))
              ;; Close of White Hammer Line must be > Open of white candlestick.
              (> (aget close# i) (aget open# (inc i)))
              true)
            ;; If the prior three intervals before the White Hammer Line are three consecutive dark
            ;; candlestick lines...
            (if (every? #{-1} (aslice direction# (inc i) (+ i 4)))
              ;; Close of White Hammer Line must be > Open of white candlestick.
              (> (aget close# i) (aget open# (inc i)))
              true))
          (aset hammer# i 1)
          ;; bear hammer
          (and
            ;(== -1 (aget zone-inside# i))
            ;; Body is black (Close < Open)
            (== -1 (aget direction# i))
            ;; Body (Close - Open) is greater than Bottom Shadow (Close - Low)
            (> (aget body-size# i)
               (aget bottom-shadow# i))
            ;; Upper shadow (High - Close) is greater than the combination of the Body and Bottom Shadow
            (> (aget upper-shadow# i)
               (+ (aget body-size# i)
                  (aget bottom-shadow# i)))
            ;; Long Upper Shadow of the White Hammer Line (c1) must have more depth (longer)
            ;; than the lower shadows of the prior three intervals.
            (aget deep-upper-shadow# i)
            ;; Low of the White Hammer Line < Lows of the prior three intervals.
            (> (aget high# i)
               (amax (aslice high# (+ i 1) (+ i 4))))
            ;; There must be a White Line with its High < Open of the White Hammer Line.
            (> (amax (aslice high# (+ i 1) (+ i 4)))
               (aget open# i))
            ;; Close of White Hammer Line > or = Lowest Low among the prior two candlestick lines.
            (>= (aget close# i)
                (amin (aslice low# (+ i 1) (+ i 3))))
            ;; If the interval before the Black Hammer Line is a white candlestick (Close < Open)...
            (if (== -1 (aget direction# (inc i)))
              ;; Close of Black Hammer Line must be < Open of black candlestick.
              (< (aget close# i) (aget open# (inc i)))
              true)
            ;; If the prior three intervals before the Black Hammer Line are three consecutive white
            ;; candlestick lines...
            (if (every? #{1} (aslice direction# (inc i) (+ i 4)))
              ;; Close of Black Hammer Line must be < Open of white candlestick.
              (< (aget close# i) (aget open# (inc i)))
              true))
          (aset hammer# i -1))

        (recur (inc i))))
    (assoc data :ajctr-hammer hammer#)))


(defn harami
  ^Quotes [^Quotes data]
  (let [array-size (alength ^objects (:time data))
        ^doubles open# (:open data)
        ^doubles high# (:high data)
        ^doubles low# (:low data)
        ^doubles close# (:close data)
        ^doubles midpoint# (:candle-body-midpoint# data)
        ^doubles body-size# (:candle-body-size# data)
        ^longs hg# (:wrb-hg-body# data)
        ^longs harami# (long-array array-size)]
    (loop [i 0]
      (when (< i (- array-size 3))
        (cond
          ;; bull harami
          (and
            ;; Dark WRB (c3) is an unfilled Hidden GAP
            (== -1 (aget hg# (+ i 2)))
            ;; the price action of c2 and c1 does not fill in the Dark WRB Hidden GAP
            (< (Math/max ^double (aget high# (inc i))
                         ^double (aget high# i))
               (aget open# (+ i 2)))
            ;; Close of White Confirmation Line (c1) > High of White Small Line (c2) or Close of
            ;; White Confirmation Line (c1) > Body midpoint of Dark WRB (c3)
            (or (> (aget close# i)
                   (aget high# (inc i)))
                (> (aget close# i)
                   (aget midpoint# (+ i 2))))
            ;; Close of White Confirmation Line (c1) < Open of Dark WRB (c3)
            (< (aget close# i)
               (aget open# (+ i 2)))
            ;; Open of White Confirmation Line (c1) > or = Close of White Small Line (c2)
            (>= (aget open# i)
                (aget close# (inc i)))
            ;; Low of White Confirmation Line (c1) > or = Low of White Small Line (c2)
            (>= (aget low# i)
                (aget low# (inc i)))
            ;; High of White Confirmation Line (c1) > Body midpoint of Dark WRB (c3)
            (> (aget high# i)
               (aget midpoint# (+ i 2)))
            ;; Close of White Small Line (c2) > Close of Dark WRB (c3)
            (> (aget close# (inc i)) (aget close# (+ i 2)))
            ;; Body of White Line (c1) or Body of White Line (c2) is smaller than the Bodies of two
            ;; of the three intervals that occurred prior to the Dark WRB Hidden GAP (c3)
            (< (Math/min ^double (aget body-size# i) ^double (aget body-size# (inc i)))
               (amin (aslice body-size# (+ i 3) (+ i 6))))
            ;; Highs of the three intervals that occurs prior to the Dark WRB (c3) > Open of Dark
            ;; WRB (c3)
            (> (amin (aslice high# (+ i 3) (+ i 6)))
               (aget open# (+ i 2)))
            ;; Bodies of the three intervals that occurs prior to the Dark WRB (c3) > or = Body
            ;; midpoint of Dark WRB (c3)
            (>= (Math/min ^double (amin (aslice open# (+ i 3) (+ i 6)))
                          ^double (amin (aslice close# (+ i 3) (+ i 6))))
                (aget midpoint# (+ i 2)))
            ;; Lows of the three intervals that occurs prior to the Dark WRB (c3) > Close of Dark
            ;; WRB (c3)
            (> (amin (aslice low# (+ i 3) (+ i 6)))
               (aget close# (+ i 2))))

          (aset harami# i 1)
          ;; bear harami
          (and
            ;; Bull WRB (c3) is an unfilled Hidden GAP
            (== 1 (aget hg# (+ i 2)))
            ;; the price action of c2 and c1 does not fill in the Dark WRB Hidden GAP
            (> (Math/min ^double (aget low# (inc i))
                         ^double (aget low# i))
               (aget open# (+ i 2)))
            ;; Close of White Confirmation Line (c1) < Low of White Small Line (c2) or Close of
            ;; White Confirmation Line (c1) < Body midpoint of Dark WRB (c3)
            (or (< (aget close# i)
                   (aget low# (inc i)))
                (< (aget close# i)
                   (aget midpoint# (+ i 2))))
            ;; Close of White Confirmation Line (c1) > Open of Dark WRB (c3)
            (> (aget close# i)
               (aget open# (+ i 2)))
            ;; Open of White Confirmation Line (c1) < or = Close of White Small Line (c2)
            (<= (aget open# i)
                (aget close# (inc i)))
            ;; High of White Confirmation Line (c1) < or = High of White Small Line (c2)
            (<= (aget high# i)
                (aget high# (inc i)))
            ;; Low of White Confirmation Line (c1) < Body midpoint of Dark WRB (c3)
            (< (aget low# i)
               (aget midpoint# (+ i 2)))
            ;; Close of White Small Line (c2) < Close of Dark WRB (c3)
            (< (aget close# (inc i)) (aget close# (+ i 2)))
            ;; Body of White Line (c1) or Body of White Line (c2) is smaller than the Bodies of two
            ;; of the three intervals that occurred prior to the Dark WRB Hidden GAP (c3)
            (< (Math/min ^double (aget body-size# i) ^double (aget body-size# (inc i)))
               (amin (aslice body-size# (+ i 3) (+ i 6))))
            ;; Lows of the three intervals that occurs prior to the Dark WRB (c3) < Open of Dark
            ;; WRB (c3)
            (< (amin (aslice low# (+ i 3) (+ i 6)))
               (aget open# (+ i 2)))
            ;; Bodies of the three intervals that occurs prior to the Dark WRB (c3) < or = Body
            ;; midpoint of Dark WRB (c3)
            (<= (Math/max ^double (amin (aslice open# (+ i 3) (+ i 6)))
                          ^double (amin (aslice close# (+ i 3) (+ i 6))))
                (aget midpoint# (+ i 2)))
            ;; Highs of the three intervals that occurs prior to the Dark WRB (c3) < Close of Dark
            ;; WRB (c3)
            (< (amin (aslice high# (+ i 3) (+ i 6)))
               (aget close# (+ i 2))))
          (aset harami# i -1))

        (recur (inc i))))
    (assoc data :ajctr-harami harami#)))


;(defn engulfing
;  ^Quotes [^Quotes data]
;  (let [array-size (alength ^objects (:time data))
;        ^doubles open# (:open data)
;        ^doubles high# (:high data)
;        ^doubles low# (:low data)
;        ^doubles close# (:close data)
;        ^longs direction# (:candle-direction# data)
;        ^doubles body-size# (:candle-body-size# data)
;        ^doubles upper-shadow# (:candle-upper-shadow# data)
;        ^doubles bottom-shadow# (:candle-bottom-shadow# data)
;        ^booleans deep-upper-shadow# (:ajctr-deep-shadow-upper? data)
;        ^booleans deep-bottom-shadow# (:ajctr-deep-shadow-bottom? data)
;        ^longs wrb# (:wrb-body# data)
;        ^longs engulfing# (long-array array-size)]
;    (loop [i 0]
;      (when (< i (- array-size 3))
;        (cond
;          ;; bull engulfing
;          (and
;            (== 1 (aget direction# i))
;            (== -1 (aget direction# (inc i)))
;            ;; Long Lower Shadow of the Dark Line (c2) must have more depth (longer) than the
;            ;; lower shadows of the prior three intervals.
;            (aget deep-bottom-shadow# (inc i))
;            (> (aget bottom-shadow# (inc i))
;               (+ (aget body-size# (inc i))
;                  (aget upper-shadow# (inc i))))
;            (< (aget low# (inc i))
;               (amin (aslice low# (+ i 2) (+ i 5))))
;            (> (aget close# i)
;               (aget open# (+ i 2)))
;            (< (aget open# i)
;               (aget close# (+ i 2)))
;            (>= (aget close# (+ i 2))
;                (aget open# (inc i)))
;            (> (aget open# (+ i 2))
;               (aget high# (inc i)))
;            (> (aget low# i)
;               (aget low# (inc i)))
;            (<= (aget close# i)
;                (amax (aslice high# (inc i) (+ i 4))))
;            (big-body-bull? i body-size# wrb#))
;          (aset engulfing# i 1)
;          ;; bear engulfing
;          (and
;            (== -1 (aget direction# i))
;            (== 1 (aget direction# (inc i)))
;            (> (aget upper-shadow# (inc i))
;               (+ (aget body-size# (inc i))
;                  (aget bottom-shadow# (inc i))))
;            (aget deep-upper-shadow# (inc i))
;            (> (aget high# (inc i))
;               (amax (aslice high# (+ i 2) (+ i 5))))
;            (< (aget close# i)
;               (aget open# (+ i 2)))
;            (> (aget open# i)
;               (aget close# (+ i 2)))
;            (<= (aget close# (+ i 2))
;                (aget open# (inc i)))
;            (< (aget open# (+ i 2))
;               (aget low# (inc i)))
;            (< (aget high# i)
;               (aget high# (inc i)))
;            (>= (aget close# i)
;                (amin (aslice low# (inc i) (+ i 4))))
;            (big-body-bear? i body-size# wrb#))
;          (aset engulfing# i -1))
;
;        (recur (inc i))))
;    (-> data
;        (assoc :ajctr-engulfing engulfing#))))

