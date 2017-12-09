(ns machine.time-management.core
  (:require [schema.core :as s]
            [clj-time.core :as dt]
            [clj-time.coerce :as dtc])
  (:import [org.joda.time DateTime]
           [clojure.lang Keyword]))


(def ^:const available-periods
  [:s5 :s10 :s15 :s30 :m1 :m2
   :m3 :m4 :m5 :m10 :m15 :m30
   :h1 :h2 :h3 :h4 :h6 :h8 :h12
   :d :w :m])


(def ^:const periods<->seconds
  (->> {:s5  5
        :s10 10
        :s15 15
        :s30 30
        :m1  60
        :m2  (* 2 60)
        :m3  (* 3 60)
        :m4  (* 4 60)
        :m5  (* 5 60)
        :m10 (* 10 60)
        :m15 (* 15 60)
        :m30 (* 30 60)
        :h1  3600
        :h2  (* 2 3600)
        :h3  (* 3 3600)
        :h4  (* 4 3600)
        :h6  (* 6 3600)
        :h8  (* 8 3600)
        :h12 (* 12 3600)
        :d   86400
        :w   (* 7 86400)
        :m   (* 30 86400)}
       (map (fn [[k v]] {k v v k}))
       (into {})))


(s/defn greater-period? :- s/Bool
        [p1 :- s/Keyword p2 :- s/Keyword]
        (> (.indexOf available-periods p1) (.indexOf available-periods p2)))


(s/defn smaler-period? :- s/Bool
        [p1 :- s/Keyword p2 :- s/Keyword]
        (< (.indexOf available-periods p1) (.indexOf available-periods p2)))


(s/defn period->seconds :- long
        [period :- s/Keyword]
        (get periods<->seconds period))


(s/defn seconds->period :- s/Keyword
        [seconds :- s/Num]
        (get periods<->seconds seconds))


(defmulti floor-time (fn [t n] [(class t) (class n)]))

(s/defmethod floor-time [DateTime Long] :- DateTime
             [t :- DateTime seconds :- long]
             (dtc/from-long (long (* (Math/floor (/ (dtc/to-long t)
                                                    (double (* seconds 1000))))
                                     (* seconds 1000)))))

(s/defmethod floor-time [DateTime Keyword] :- DateTime
             [t :- DateTime period :- s/Keyword]
             (floor-time t (period->seconds period)))


(s/defn shift-period :- s/Keyword
  [period :- s/Keyword n :- long]
  (get available-periods (+ (.indexOf available-periods period) n)))


(s/defn upper-period :- s/Keyword
  [period :- s/Keyword]
  (shift-period period 1))


(s/defn lower-period :- s/Keyword
  [period :- s/Keyword]
  (shift-period period -1))


(s/defn candle-time :- DateTime
  ([period :- s/Keyword]
    (candle-time period (dt/now)))
  ([period :- s/Keyword t :- DateTime]
    (floor-time t (period->seconds period))))


(s/defn previous-candle-time :- DateTime
  ([period :- s/Keyword t :- DateTime]
    (dt/minus (floor-time t period) (dt/seconds (period->seconds period))))
  ([period :- s/Keyword]
    (dt/minus (candle-time period) (dt/seconds (period->seconds period)))))


(s/defn next-candle-time :- DateTime
  ([period :- s/Keyword t :- DateTime]
    (dt/plus (floor-time t period) (dt/seconds (period->seconds period))))
  ([period :- s/Keyword]
    (dt/plus (candle-time period) (dt/seconds (period->seconds period)))))