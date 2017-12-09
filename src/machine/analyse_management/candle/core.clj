(ns machine.analyse-management.candle.core
  (:require [clj-time.coerce :as dtc]
            [machine.time-management.core :as tm])
  (:import [clojure.lang Keyword]
           [machine.quotes.core Quotes]
           [org.apache.commons.math3.util FastMath]))


(set! *unchecked-math* true)


(defprotocol Candle
  (direction [data] [data i])
  (upper-period-direction [data period] [data period i])
  (consecutive-direction [data] [data i])
  (body-midpoint [data] [data i])
  (body-size [data] [data i])
  (bar-midpoint [data] [data i])
  (bar-size [data] [data i])
  (broken-body-size [data] [data i])
  (broken-bar-size [data] [data i])
  (broken-volume [data] [data i])
  (upper-shadow [data] [data i])
  (bottom-shadow [data] [data i])
  (broken-range [data] [data i])
  (filled-by [data] [data i])
  (high-broken-by [data] [data i])
  (low-broken-by [data] [data i])
  (hidden-gap [data] [data i])
  (fractal-high [data n] [data n i])
  (fractal-low [data n] [data n i])
  (prior-fractal-high [data] [data i])
  (prior-fractal-low [data] [data i])
  (prior-unbroken-fractal-high [data] [data i])
  (prior-unbroken-fractal-low [data] [data i])
  (fractal-break [data] [data i])
  (reaction [data] [data i])
  (prior-reaction-high [data] [data i])
  (prior-reaction-low [data] [data i])
  (prior-unbroken-reaction-high [data] [data i])
  (prior-unbroken-reaction-low [data] [data i])
  (consolidation [data n m] [data n m i])
  )



(extend-type Quotes
  Candle
  (direction
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles close#] :as data}]
     (let [array-size (alength time#)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (let [^long dir (cond (> (aget open# i) (aget close# i)) -1
                                 (< (aget open# i) (aget close# i)) 1
                                 :else 0)]
             (aset result i dir)
             (recur (inc i)))))
       (assoc data :candle-direction# result)))
    (^long [{:keys [^longs candle-direction#] :as data} ^long i]
     (if candle-direction#
       (aget candle-direction# i)
       (recur (direction data) i))))
  (upper-period-direction
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles close#] :as data} ^Keyword period]
     {:pre [((set tm/available-periods) period)]}
     (let [array-size (alength time#)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (loop [j 0]
             (when (< (+ i j) array-size)
               (if (== 0 (rem (/ (dtc/to-long (aget time# (+ i j))) 1000) (tm/period->seconds period)))
                 (let [dir (cond (> (aget open# (+ i j)) (aget close# i)) -1
                                 (< (aget open# (+ i j)) (aget close# i)) 1
                                 :else 0)]
                   (aset result i ^long dir))
                 (recur (inc j)))))
           (recur (inc i))))
       (assoc data (keyword (str "candle-" (name period) "-direction#")) result))))
  (consecutive-direction
    (^Quotes [{:keys [^objects time# ^longs candle-direction#] :as data}]
     (let [array-size (alength time#)
           result (long-array array-size)]
       (loop [i (dec array-size) last-dir 0 cnt 1]
         (when (>= i 0)
           (let [cnt (case (aget candle-direction# i)
                       1 (if (== 1 last-dir) (inc cnt) 1)
                       -1 (if (== -1 last-dir) (dec cnt) -1)
                       0 0)]
             (aset result i cnt)
             (recur (dec i) (aget candle-direction# i) cnt))))
       (assoc data :candle-consecutive-direction# result)))
    (^long [{:keys [^longs candle-consecutive-direction#] :as data} ^long i]

     (if candle-consecutive-direction#
       (aget candle-consecutive-direction# i)
       (recur (consecutive-direction data) i))))
  (body-size
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles close#] :as data}]
     (let [array-size (alength time#)
           result (double-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (let [size (FastMath/abs (- (aget open# i) (aget close# i)))]
             (aset result i size)
             (recur (inc i)))))
       (assoc data :candle-body-size# result)))
    (^double [{:keys [^longs candle-body-size#] :as data} ^long i]
     (if candle-body-size#
       (aget candle-body-size# i)
       (recur (body-size data) i))))
  (broken-body-size
    (^Quotes [{:keys [^objects time# ^doubles candle-body-size#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (and (< (+ i j) array-size)
                      (> (aget candle-body-size# i)
                         (aget candle-body-size# (+ i j))))
               (recur (inc j))
               (aset result i (dec j))))

           (recur (inc i))))
       (assoc data :candle-broken-body-size# result)))
    (^long [{:keys [^longs candle-broken-body-size#] :as data} ^long i]
     (if candle-broken-body-size#
       (aget candle-broken-body-size# i)
       (recur (broken-body-size data) i))))
  (body-midpoint
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles close#] :as data}]
     (let [array-size (alength time#)
           result (double-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (aset result i (* 0.5 (+ (aget open# i) (aget close# i))))
           (recur (inc i))))
       (assoc data :candle-body-midpoint# result)))
    (^double [{:keys [^longs candle-body-midpoint#] :as data} ^long i]
     (if candle-body-midpoint#
       (aget candle-body-midpoint# i)
       (recur (body-midpoint data) i))))
  (bar-size
    (^Quotes [{:keys [^objects time# ^doubles high# ^doubles low#] :as data}]
     (let [array-size (alength time#)
           result (double-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (let [size (- (aget high# i) (aget low# i))]
             (aset result i size)
             (recur (inc i)))))
       (assoc data :candle-bar-size# result)))
    (^double [{:keys [^longs candle-bar-size#] :as data} ^long i]
     (if candle-bar-size#
       (aget candle-bar-size# i)
       (recur (bar-size data) i))))
  (broken-bar-size
    (^Quotes [{:keys [^objects time# ^doubles candle-bar-size#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (and (< (+ i j) array-size)
                      (> (aget candle-bar-size# i)
                         (aget candle-bar-size# (+ i j))))
               (recur (inc j))
               (aset result i (dec j))))

           (recur (inc i))))
       (assoc data :candle-broken-bar-size# result)))
    (^long [{:keys [^longs candle-broken-bar-size#] :as data} ^long i]
     (if candle-broken-bar-size#
       (aget candle-broken-bar-size# i)
       (recur (broken-body-size data) i))))
  (bar-midpoint
    (^Quotes [{:keys [^objects time# ^doubles high# ^doubles low#] :as data}]
     (let [array-size (alength time#)
           result (double-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (aset result i (- (aget high# i) (aget low# i)))
           (recur (inc i))))
       (assoc data :candle-bar-midpoint# result)))
    (^double [{:keys [^longs candle-bar-midpoint#] :as data} ^long i]
     (if candle-bar-midpoint#
       (aget candle-bar-midpoint# i)
       (recur (bar-midpoint data) i))))
  (broken-range
    (^Quotes [{:keys [^objects time# ^doubles high# ^doubles low# ^doubles close#
                      ^longs candle-direction#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (case (aget candle-direction# i)
             1 (loop [j 1]
                 (if (and (< (+ i j) array-size)
                          (> (aget close# i) (aget high# (+ i j))))
                   (recur (inc j))
                   (aset result i (dec j))))

             -1 (loop [j 1]
                  (if (and (< (+ i j) array-size)
                           (< (aget close# i) (aget low# (+ i j))))
                    (recur (inc j))
                    (aset result i (dec j))))
             nil)
           (recur (inc i))))
       (assoc data :candle-broken-range# result)))
    (^long [{:keys [^longs candle-broken-range#] :as data} ^long i]
     (if candle-broken-range#
       (aget candle-broken-range# i)
       (recur (broken-range data) i))))
  (upper-shadow
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles high# ^doubles close#] :as data}]
     (let [array-size (alength time#)
           result (double-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (let [open# (aget open# i)
                 high# (aget high# i)
                 close# (aget close# i)
                 size (- high# (FastMath/max open# close#))]
             (aset result i size)
             (recur (inc i)))))
       (assoc data :candle-upper-shadow# result)))
    (^double [{:keys [^longs candle-upper-shadow#] :as data} ^long i]
     (if candle-upper-shadow#
       (aget candle-upper-shadow# i)
       (recur (upper-shadow data) i))))
  (bottom-shadow
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles low# ^doubles close#] :as data}]
     (let [array-size (alength time#)
           result (double-array array-size)]
       (loop [i 0]
         (when (< i array-size)
           (let [open# (aget open# i)
                 low# (aget low# i)
                 close# (aget close# i)
                 size (- (FastMath/min open# close#) low#)]
             (aset result i size)
             (recur (inc i)))))
       (assoc data :candle-bottom-shadow# result)))
    (^double [{:keys [^longs candle-bottom-shadow#] :as data} ^long i]
     (if candle-bottom-shadow#
       (aget candle-bottom-shadow# i)
       (recur (bottom-shadow data) i))))
  (broken-volume
    (^Quotes [{:keys [^objects time# ^longs volume#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (and (< (+ i j) array-size)
                      (> (aget volume# i) (aget volume# (+ i j))))
               (recur (inc j))
               (aset result i (dec j))))
           (recur (inc i))))
       (assoc data :candle-broken-volume# result)))
    (^long [{:keys [^longs candle-broken-volume#] :as data} ^long i]
     (if candle-broken-volume#
       (aget candle-broken-volume# i)
       (recur (broken-volume data) i))))
  (filled-by
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles high# ^doubles low#
                      ^longs candle-direction#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (case (aget candle-direction# i)
             1 (loop [j 1]
                 (if (<= j i)
                   (if (<= (aget low# (- i j)) (aget open# i))
                     (aset result i (- i j))
                     (recur (inc j)))
                   (aset result i -1)))
             -1 (loop [j 1]
                  (if (<= j i)
                    (if (>= (aget high# (- i j)) (aget open# i))
                      (aset result i (- i j))
                      (recur (inc j)))
                    (aset result i -1)))
             nil)
           (recur (inc i))))
       (assoc data :candle-filled-by# result)))
    (^long [{:keys [^longs candle-filled-by#] :as data} ^long i]
     (if candle-filled-by#
       (aget candle-filled-by# i)
       (recur (filled-by data) i))))
  (high-broken-by
    (^Quotes [{:keys [^objects time# ^doubles high# ^doubles close#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (<= j i)
               (if (> (aget close# (- i j))
                      (aget high# i))
                 (aset result i (- i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-high-broken-by# result)))
    (^long [{:keys [^longs candle-high-broken-by#] :as data} ^long i]
     (if candle-high-broken-by#
       (aget candle-high-broken-by# i)
       (recur (high-broken-by data) i))))
  (low-broken-by
    (^Quotes [{:keys [^objects time# ^doubles low# ^doubles close#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (<= j i)
               (if (< (aget close# (- i j))
                      (aget low# i))
                 (aset result i (- i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-low-broken-by# result)))
    (^long [{:keys [^longs candle-low-broken-by#] :as data} ^long i]
     (if candle-low-broken-by#
       (aget candle-low-broken-by# i)
       (recur (low-broken-by data) i))))
  (hidden-gap
    (^Quotes [{:keys [^objects time# ^doubles high# ^doubles low#
                      ^longs candle-direction#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (double-array array-size)]
       (loop [i 1]
         (when (< i loop-limit)
           (let [gap (case (aget candle-direction# i)
                       1 (- (aget low# (dec i))
                            (aget high# (inc i)))
                       -1 (- (aget low# (inc i))
                             (aget high# (dec i)))
                       0 (FastMath/max ^double (- (aget low# (dec i)) (aget high# (inc i)))
                                       ^double (- (aget low# (inc i)) (aget high# (dec i)))))]
             (aset result i gap))
           (recur (inc i))))
       (assoc data :candle-hidden-gap# result)))
    (^double [{:keys [^longs candle-hidden-gap#] :as data} ^long i]
     (if candle-hidden-gap#
       (aget candle-hidden-gap# i)
       (recur (hidden-gap data) i))))
  (fractal-high
    (^Quotes [{:keys [^objects time# ^doubles high#] :as data} ^long n]
     (let [array-size (alength time#)
           loop-limit (- array-size n)
           result (boolean-array array-size)]
       (loop [i n]
         (when (< i loop-limit)
           (loop [j 1]
             (if (<= j n)
               (when (and (> (aget high# i) (aget high# (+ i j)))
                          (> (aget high# i) (aget high# (- i j))))
                 (recur (inc j)))
               (aset result i true)))
           (recur (inc i))))
       (assoc data :candle-fractal-high# result)))
    (^double [{:keys [^longs candle-fractal-high#] :as data} ^long n ^long i]
     (if candle-fractal-high#
       (aget candle-fractal-high# i)
       (recur (fractal-high data n) n i))))
  (fractal-low
    (^Quotes [{:keys [^objects time# ^doubles low#] :as data} ^long n]
     (let [array-size (alength time#)
           loop-limit (- array-size n)
           result (boolean-array array-size)]
       (loop [i n]
         (when (< i loop-limit)
           (loop [j 1]
             (if (<= j n)
               (when (and (< (aget low# i) (aget low# (+ i j)))
                          (< (aget low# i) (aget low# (- i j))))
                 (recur (inc j)))
               (aset result i true)))
           (recur (inc i))))
       (assoc data :candle-fractal-low# result)))
    (^double [{:keys [^longs candle-fractal-low#] :as data} ^long n ^long i]
     (if candle-fractal-low#
       (aget candle-fractal-low# i)
       (recur (fractal-low data n) n i))))
  (prior-fractal-high
    (^Quotes [{:keys [^objects time# ^booleans candle-fractal-high#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (aget candle-fractal-high# (+ i j))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-prior-fractal-high# result)))
    (^double [{:keys [^longs candle-prior-fractal-high#] :as data} ^long i]
     (if candle-prior-fractal-high#
       (aget candle-prior-fractal-high# i)
       (recur (prior-fractal-high data) i))))
  (prior-fractal-low
    (^Quotes [{:keys [^objects time# ^booleans candle-fractal-low#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (aget candle-fractal-low# (+ i j))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-prior-fractal-low# result)))
    (^double [{:keys [^longs candle-prior-fractal-low#] :as data} ^long i]
     (if candle-prior-fractal-low#
       (aget candle-prior-fractal-low# i)
       (recur (prior-fractal-low data) i))))
  (prior-unbroken-fractal-high
    (^Quotes [{:keys [^objects time# ^booleans candle-fractal-high#
                      ^longs candle-high-broken-by#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (and (aget candle-fractal-high# (+ i j))
                        (<= (aget candle-high-broken-by# (+ i j)) i))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-prior-unbroken-fractal-high# result)))
    (^double [{:keys [^longs candle-prior-unbroken-fractal-high#] :as data} ^long i]
     (if candle-prior-unbroken-fractal-high#
       (aget candle-prior-unbroken-fractal-high# i)
       (recur (prior-unbroken-fractal-high data) i))))
  (prior-unbroken-fractal-low
    (^Quotes [{:keys [^objects time# ^booleans candle-fractal-low#
                      ^longs candle-low-broken-by#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (and (aget candle-fractal-low# (+ i j))
                        (<= (aget candle-low-broken-by# (+ i j)) i))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-prior-unbroken-fractal-low# result)))
    (^double [{:keys [^longs candle-prior-unbroken-fractal-low#] :as data} ^long i]
     (if candle-prior-unbroken-fractal-low#
       (aget candle-prior-unbroken-fractal-low# i)
       (recur (prior-unbroken-fractal-low data) i))))
  (fractal-break
    (^Quotes [{:keys [^objects time# ^doubles open# ^doubles high# ^doubles low#
                      ^doubles close# ^longs candle-direction#
                      ^booleans candle-fractal-high#
                      ^booleans candle-fractal-low#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           fractal-break# (long-array array-size)
           fractal-break-nr# (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (case (aget candle-direction# i)
             1 (loop [j 1]
                 (when (and (< (+ i j) array-size)
                            (> (aget close# i) (aget high# (+ i j))))
                   (if (and (aget candle-fractal-high# (+ i j))
                            (< (aget open# i) (aget high# (+ i j))))
                     (do (aset fractal-break# i 1)
                         (aset fractal-break-nr# i (+ i j)))
                     (recur (inc j)))))

             -1 (loop [j 1]
                  (when (and (< (+ i j) array-size)
                             (< (aget close# i) (aget low# (+ i j))))
                    (if (and (aget candle-fractal-low# (+ i j))
                             (> (aget open# i) (aget low# (+ i j))))
                      (do (aset fractal-break# i -1)
                          (aset fractal-break-nr# i (+ i j)))
                      (recur (inc j)))))
             nil)
           (recur (inc i))))
       (assoc data :candle-fractal-break# fractal-break#
                   :candle-fractal-break-nr# fractal-break-nr#)))
    (^double [{:keys [^longs candle-fractal-break#] :as data} ^long i]
     (if candle-fractal-break#
       (aget candle-fractal-break# i)
       (recur (fractal-break data) i))))                    ;FIXME fractal-break-nr
  (reaction
    (^Quotes [{:keys [^objects time# ^doubles high# ^doubles low# ^doubles close#
                      ^longs candle-direction#] :as data}]
     (let [array-size (alength time#)
           loop-limit (- array-size 2)
           result (long-array array-size)]
       (loop [i 2]
         (when (< i loop-limit)
           (loop [j 2 after false before false]
             (if (and after before)
               (aset result i 1)
               (when (and (<= j i) (< j (- array-size i)))
                 (when (and (< (aget high# (- i j)) (aget high# i))
                            (< (aget high# (inc (- i j))) (aget high# i))
                            (< (aget high# (+ i j)) (aget high# i))
                            (< (aget high# (dec (+ i j))) (aget high# i)))
                   (cond
                     (and (< (aget close# (- i j)) (aget close# (inc (- i j))))
                          (< (aget close# (- i j)) (aget close# i))
                          (== -1 (aget candle-direction# (- i j)))
                          (< (aget close# (inc (- i j))) (aget close# i))
                          (== -1 (aget candle-direction# (inc (- i j))))
                          (< (aget close# (+ i j)) (aget close# (dec (+ i j))))
                          (< (aget close# (+ i j)) (aget close# i))
                          (== 1 (aget candle-direction# (+ i j)))
                          (< (aget close# (dec (+ i j))) (aget close# i))
                          (== 1 (aget candle-direction# (dec (+ i j)))))
                     (recur (inc j) true true)

                     (and (< (aget close# (- i j)) (aget close# (inc (- i j))))
                          (< (aget close# (- i j)) (aget close# i))
                          (== -1 (aget candle-direction# (- i j)))
                          (< (aget close# (inc (- i j))) (aget close# i))
                          (== -1 (aget candle-direction# (inc (- i j)))))
                     (recur (inc j) true before)

                     (and (< (aget close# (+ i j)) (aget close# (dec (+ i j))))
                          (< (aget close# (+ i j)) (aget close# i))
                          (== 1 (aget candle-direction# (+ i j)))
                          (< (aget close# (dec (+ i j))) (aget close# i))
                          (== 1 (aget candle-direction# (dec (+ i j)))))
                     (recur (inc j) after true))))))
           (loop [j 2 after false before false]
             (if (and after before)
               (aset result i -1)
               (when (and (<= j i) (< j (- array-size i)))
                 (when (and (> (aget low# (- i j)) (aget low# i))
                            (> (aget low# (inc (- i j))) (aget low# i))
                            (> (aget low# (+ i j)) (aget low# i))
                            (> (aget low# (dec (+ i j))) (aget low# i)))
                   (cond
                     (and (> (aget close# (- i j)) (aget close# (inc (- i j))))
                          (> (aget close# (- i j)) (aget close# i))
                          (== 1 (aget candle-direction# (- i j)))
                          (> (aget close# (inc (- i j))) (aget close# i))
                          (== 1 (aget candle-direction# (inc (- i j))))
                          (> (aget close# (+ i j)) (aget close# (dec (+ i j))))
                          (> (aget close# (+ i j)) (aget close# i))
                          (== -1 (aget candle-direction# (+ i j)))
                          (> (aget close# (dec (+ i j))) (aget close# i))
                          (== -1 (aget candle-direction# (dec (+ i j)))))
                     (recur (inc j) true true)

                     (and (> (aget close# (- i j)) (aget close# (inc (- i j))))
                          (> (aget close# (- i j)) (aget close# i))
                          (== 1 (aget candle-direction# (- i j)))
                          (> (aget close# (inc (- i j))) (aget close# i))
                          (== 1 (aget candle-direction# (inc (- i j)))))
                     (recur (inc j) true before)

                     (and (> (aget close# (+ i j)) (aget close# (dec (+ i j))))
                          (> (aget close# (+ i j)) (aget close# i))
                          (== -1 (aget candle-direction# (+ i j)))
                          (> (aget close# (dec (+ i j))) (aget close# i))
                          (== -1 (aget candle-direction# (dec (+ i j)))))
                     (recur (inc j) after true))))))
           (recur (inc i))))
       (assoc data :candle-reaction# result)))
    (^double [{:keys [^longs candle-reaction#] :as data} ^long i]
     (if candle-reaction#
       (aget candle-reaction# i)
       (recur (reaction data) i))))
  (prior-reaction-high
    (^Quotes [{:keys [^objects time# ^longs candle-reaction#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (== 1 (aget candle-reaction# (+ i j)))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-prior-reaction-high# result)))
    (^double [{:keys [^longs candle-prior-reaction-high#] :as data} ^long i]
     (if candle-prior-reaction-high#
       (aget candle-prior-reaction-high# i)
       (recur (prior-reaction-high data) i))))
  (prior-reaction-low
    (^Quotes [{:keys [^objects time# ^longs candle-reaction#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (== -1 (aget candle-reaction# (+ i j)))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-prior-reaction-low# result)))
    (^double [{:keys [^longs candle-prior-reaction-low#] :as data} ^long i]
     (if candle-prior-reaction-low#
       (aget candle-prior-reaction-low# i)
       (recur (prior-reaction-low data) i))))
  (prior-unbroken-reaction-high
    (^Quotes [{:keys [^objects time# ^longs candle-reaction#
                      ^longs candle-high-broken-by#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (and (== 1 (aget candle-reaction# (+ i j)))
                        (<= (aget candle-high-broken-by# (+ i j)) i))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-prior-unbroken-reaction-high# result)))
    (^double [{:keys [^longs candle-prior-unbroken-reaction-high#] :as data} ^long i]
     (if candle-prior-unbroken-reaction-high#
       (aget candle-prior-unbroken-reaction-high# i)
       (recur (prior-unbroken-reaction-high data) i))))
  (prior-unbroken-reaction-low
    (^Quotes [{:keys [^objects time# ^longs candle-reaction#
                      ^longs candle-low-broken-by#] :as data}]
     (let [array-size (alength time#)
           loop-limit (dec array-size)
           result (long-array array-size)]
       (loop [i 0]
         (when (< i loop-limit)
           (loop [j 1]
             (if (< (+ i j) array-size)
               (if (and (== -1 (aget candle-reaction# (+ i j)))
                        (<= (aget candle-low-broken-by# (+ i j)) i))
                 (aset result i (+ i j))
                 (recur (inc j)))
               (aset result i -1)))
           (recur (inc i))))
       (assoc data :candle-prior-unbroken-reaction-low# result)))
    (^double [{:keys [^longs candle-prior-unbroken-reaction-low#] :as data} ^long i]
     (if candle-prior-unbroken-reaction-low#
       (aget candle-prior-unbroken-reaction-low# i)
       (recur (prior-unbroken-reaction-low data) i)))))


(defn analyse
  ^Quotes [^Quotes data]
  (-> data
      (direction)
      (consecutive-direction)
      (body-size)
      (broken-body-size)
      (body-midpoint)
      (bar-size)
      (broken-bar-size)
      (bar-midpoint)
      (broken-volume)
      (upper-shadow)
      (bottom-shadow)
      (broken-range)
      (filled-by)
      (high-broken-by)
      (low-broken-by)
      (hidden-gap)
      (fractal-high 5)
      (fractal-low 5)
      (prior-fractal-high)
      (prior-fractal-low)
      (prior-unbroken-fractal-high)
      (prior-unbroken-fractal-low)
      (fractal-break)
      ;(reaction)
      ;(prior-reaction-high)
      ;(prior-reaction-low)
      ;(prior-unbroken-reaction-high)
      ;(prior-unbroken-reaction-low)
      ))

;(defn analyse-profile
;  [^Quotes data]
;  (taoensso.timbre.profiling/p :direction (direction data))
;  (taoensso.timbre.profiling/p :consecutive-direction (consecutive-direction data))
;  (taoensso.timbre.profiling/p :body-midpoint (body-midpoint data))
;  (taoensso.timbre.profiling/p :body-size (body-size data))
;  (taoensso.timbre.profiling/p :body-size-mean (body-size-mean data))
;  (taoensso.timbre.profiling/p :bar-midpoint (bar-midpoint data))
;  (taoensso.timbre.profiling/p :bar-size (bar-size data))
;  (taoensso.timbre.profiling/p :broken-body-size (broken-body-size data))
;  (taoensso.timbre.profiling/p :upper-shadow (upper-shadow data))
;  (taoensso.timbre.profiling/p :bottom-shadow (bottom-shadow data))
;  (taoensso.timbre.profiling/p :shadow-size-mean (shadow-size-mean data))
;  (taoensso.timbre.profiling/p :broken-bar (broken-bar data))
;  (taoensso.timbre.profiling/p :filled-by (filled-by data))
;  (taoensso.timbre.profiling/p :high-broken-by (high-broken-by data))
;  (taoensso.timbre.profiling/p :low-broken-by (low-broken-by data))
;  (taoensso.timbre.profiling/p :hidden-gap (hidden-gap data))
;  (taoensso.timbre.profiling/p :fractal (fractal data))
;  (taoensso.timbre.profiling/p :prior-fractal (prior-fractal data))
;  (taoensso.timbre.profiling/p :prior-unbroken-fractal (prior-unbroken-fractal data))
;  (taoensso.timbre.profiling/p :fractal-break (fractal-break data)))