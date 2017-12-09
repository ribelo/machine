(ns machine.analyse-management.market-hours.core
  (:require [clj-time.core :as dt]
            [machine.quotes.series :refer :all]
            [machine.time-management.market-hours :as mh]))



(defn week-day [data]
  (let [array-size (alength ^objects (:time data))
        ^objects timestamp# (:time data)
        ^longs result# (long-array array-size)]
    (loop [i 0]
      (when (< i array-size)
        (aset result# i ^long (dt/day-of-week (aget timestamp# i)))
        (recur (inc i))))
    (assoc data :datetime-week-day result#)))


(defn month-day [data]
  (let [array-size (alength ^objects (:time data))
        ^objects timestamp# (:time data)
        ^longs result# (long-array array-size)]
    (loop [i 0]
      (when (< i array-size)
        (aset result# i ^long (dt/day (aget timestamp# i)))
        (recur (inc i))))
    (assoc data :datetime-month-day result#)))


(defn market-hours [data]
  (let [array-size (alength ^objects (:time data))
        ^objects timestamp# (:time data)
        ^booleans london (boolean-array array-size)
        ^booleans frankfurt (boolean-array array-size)
        ^booleans new-york (boolean-array array-size)
        ^booleans chicago (boolean-array array-size)
        ^booleans tokyo (boolean-array array-size)
        ^booleans hong-kong (boolean-array array-size)
        ^booleans sydney (boolean-array array-size)
        ^booleans wellington (boolean-array array-size)]
    (loop [i 0]
      (when (< i array-size)
        (let [timestamp (aget timestamp# i)]
          (aset london i (boolean (mh/london-market? timestamp)))
          (aset frankfurt i (boolean (mh/frankfurt-market? timestamp)))
          (aset new-york i (boolean (mh/new-york-market? timestamp)))
          (aset chicago i (boolean (mh/chicago-market? timestamp)))
          (aset tokyo i (boolean (mh/tokyo-market? timestamp)))
          (aset hong-kong i (boolean (mh/hong-kong-market? timestamp)))
          (aset sydney i (boolean (mh/sydney-market? timestamp)))
          (aset wellington i (boolean (mh/wellington-market? timestamp)))
          (recur (inc i)))))
    (-> data
        (assoc :market-london? london)
        (assoc :market-frankfurt? frankfurt)
        (assoc :market-new-york? new-york)
        (assoc :market-chicago? chicago)
        (assoc :market-tokyo? tokyo)
        (assoc :market-hong-kong? hong-kong)
        (assoc :market-sydney? sydney)
        (assoc :market-wellington? wellington))))


(defn analyse [data]
  (-> data
      week-day
      month-day
      market-hours))
