(ns machine.quotes.core
  (:refer-clojure :exclude [distinct get set!])
  (:require [taoensso.encore :refer [distinct-by]]
            [machine.time-management.core :as tim]
            [clj-time.core :as dt]
            [clj-time.periodic :as dtp]
            [taoensso.timbre :as timbre]
            [machine.quotes.series :refer :all]
            [machine.time-management.core :refer [period->seconds]]
            [machine.time-management.market-hours :refer [within-trading-hours?]]
            [schema.core :as s])
  (:import [clojure.lang Keyword PersistentVector PersistentArrayMap]
           [org.apache.commons.lang3 ArrayUtils]
           [org.joda.time DateTime]))

(defrecord Quotes
  [^Keyword instrument
   ^Keyword period
   ^"[Lorg.joda.time.DateTime;" time#
   ^doubles open#
   ^doubles high#
   ^doubles low#
   ^doubles close#
   ^longs volume#
   ^doubles open-bid#
   ^doubles high-bid#
   ^doubles low-bid#
   ^doubles close-bid#
   ^doubles open-ask#
   ^doubles high-ask#
   ^doubles low-ask#
   ^doubles close-ask#
   ^doubles spread#])


(defn type-of [v]
  (condp = (type v)
    Double Double/TYPE
    Long Long/TYPE
    Integer Long/TYPE
    Boolean Boolean/TYPE
    DateTime DateTime
    Object))

(defn type-of-arr [v]
  (condp = (type v)
    Double doubles
    Long longs
    Integer longs
    Boolean booleans
    nil))


(s/defn rows->columns :- {s/Keyword s/Any}
  [coll :- [{s/Keyword s/Any}]]
  (into {}
        (map (fn [column]
               (let [array-type (type-of (clojure.core/get (first coll) column))
                     values (mapv #(clojure.core/get % column) coll)]
                 {(keyword column) (into-array array-type values)}))
             (keys (first coll)))))

(s/defn rows->vectors :- {s/Keyword [s/Any]}
  [coll :- [{s/Keyword s/Any}]]
  (into {}
        (map (fn [column]
               (let [values (mapv #(clojure.core/get % column) coll)]
                 {(keyword column) values}))
             (keys (first coll)))))

(s/defn columns->rows :- [{s/Keyword s/Any}]
  [m :- {s/Keyword s/Any}]
  (mapv (fn [v] (zipmap (keys m) v))
        (apply map vector (vals m))))


(s/defn from-columns :- Quotes
  [instrument :- s/Keyword period :- s/Keyword m :- {s/Keyword s/Any}]
  {:pre [(every? (set (keys m)) [:time# :open# :high# :low# :close# :volume#
                                 :open-bid# :high-bid# :low-bid# :close-bid#
                                 :open-ask# :high-ask# :low-ask# :close-ask#])
         (every? = (map (fn [[_ v]] (count v)) m))
         (apply distinct? (:time# m))]}
  (map->Quotes
    (-> (keys m)
        (->> (map (fn [column]
                    (let [array-type (type-of (first (clojure.core/get m column)))]
                      {(keyword column) (into-array (clojure.core/get m column) array-type)}))))
        (into {})
        (assoc :instrument instrument
               :period period))))


(s/defn from-rows :- Quotes
  [coll :- [{s/Keyword s/Any}]]
  {:pre [(every? true? (map (fn [row] (every? (set (keys row)) [:time# :open# :high# :low# :close# :volume#
                                                                :open-bid# :high-bid# :low-bid# :close-bid#
                                                                :open-ask# :high-ask# :low-ask# :close-ask#]))
                            coll))
         (every? true? (map (fn [row] (every? (set (keys row)) (keys (first coll))))
                            coll))
         ;(apply distinct? (map :time coll))
         ]}
  (map->Quotes
    (assoc (rows->columns coll)
      :instrument (get-in coll [0 :instrument])
      :period (get-in coll [0 :period]))))


;(defn new-quotes [columns length]
;  {:pre [(every? (set columns) [:_id :instrument :period :time
;                                :open :high :low :close :volume])]}
;  (map->Data
;    (into {} (map (fn [column]
;                    {column (vec (take length (repeat nil)))})
;               columns))))



(defprotocol PQData
  (data [quotes])
  (element-count [quotes])
  (get [quotes row column])
  (set! [quotes row column val])
  (get-row [quotes row])
  (get-column [quotes column])
  (subvector [quotes start] [quotes start end])
  (add-row [quotes] [quotes row])
  (replace-row [quotes row])
  (add-column [quotes column-name column])
  (replace-column [quotes column-name column])
  (column-names [quotes])
  (columns [quotes])
  (select-columns [quotes columns])
  (select-rows [quotes rows])
  (to-map [quotes])
  (row-maps [quotes])
  (shift [quotes shift])
  (join [q1 q1])
  (distinct [quotes])
  (cleanup [quotes])
  (cleanup-hours [quotes])
  (fill-gaps [quotes] [quotes period]))


(defprotocol PQDataTime
  (period [quotes])
  (last-candle-time [quotes])
  (next-candle-time [quotes]))


(extend-type Quotes
  PQData
  (element-count ^long [quotes]
    (alength ^doubles (:open# quotes)))

  (get [quotes ^long row ^Keyword column]
    (when-let [arr (clojure.core/get quotes column)]
      (aget arr row)))

  ;(set! [quotes ^long row ^Keyword column v]
  ;  (let [arr (clojure.core/get quotes column)
  ;        f (condp = (first arr)
  ;            Double doubles
  ;            Long longs
  ;            Boolean booleans)]
  ;    (aset (f (clojure.core/get quotes column)) row v)))

  (get-row
    ^PersistentArrayMap [quotes ^long row]
    (-> (keys quotes)
        (->> (remove #(#{:instrument :period} %))
             (map (fn [column]
                    {column (aget (clojure.core/get quotes column) row)}))
             (into {}))
        (assoc :instrument (:instrument quotes)
               :period (:period quotes))))

  (get-column
    [quotes ^Keyword column]
    (clojure.core/get quotes column))

  (subvector
    (^Quotes [quotes ^long start]
     (map->Quotes
       (-> (keys quotes)
           (->> (filter #(re-find #"\#" (name %)))
                (map (fn [column]
                       {column (aslice (clojure.core/get quotes column) start)}))
                (into {}))
           (assoc :instrument (:instrument quotes)
                  :period (:period quotes)))))
    (^Quotes [quotes ^long start ^long end]
     (map->Quotes
       (-> (keys quotes)
           (->> (filter #(re-find #"\#" (name %)))
                (map (fn [column]
                       {column (aslice (clojure.core/get quotes column) start end)}))
                (into {}))
           (assoc :instrument (:instrument quotes)
                  :period (:period quotes))))))

  ;(add-column [quotes ^Keyword column-name ^PersistentVector column]
  ;  (assoc-in quotes column-name column))

  ;(column-names [quotes]
  ;  (keys quotes))

  ;(columns [quotes]
  ;  (mapv (fn [column]
  ;          (clojure.core/get quotes column))
  ;        (keys quotes)))

  ;(select-columns [quotes columns]
  ;  (map->Quotes
  ;    (into {}
  ;          (map (fn [column]
  ;                 {column (clojure.core/get quotes column)})
  ;               columns))))

  ;(select-rows [quotes rows]
  ;  (mapv (fn [row]
  ;          (get-row quotes row))
  ;        rows))

  (to-map ^PersistentArrayMap
  [quotes]
    (into {} quotes))

  (row-maps ^PersistentVector
  [quotes]
    (let [instrument (:instrument quotes)
          period (:period quotes)]
      (-> quotes
          (dissoc :instrument :period)
          (columns->rows)
          (->> (map (fn [m] (assoc m :instrument instrument
                                     :period period)))
               (sort-by :time#))
          (reverse))))

  (join
    ^Quotes [q1 q2]
    {:pre [(= (set (keys q1)) (set (keys q2)))]}
    (let [t1 (:time# q1)
          t2 (:time# q2)
          idxs (->> (zipmap (range) t2)
                    (map (fn [[idx val]] (when (ArrayUtils/contains ^objects t1 val) idx)))
                    (into [] (comp (remove nil?))))
          new-array-size (- (+ (alength t1) (alength t2)) (count idxs))]
      (map->Quotes
        (-> (keys q1)
            (->> (remove #(#{:instrument :period} %))
                 (mapv (fn [column]
                        (let [arr1 (clojure.core/get q1 column)
                              arr2 (clojure.core/get q2 column)
                              joined (make-array (type-of (first arr1)) new-array-size)]
                          (when (seq idxs) (ArrayUtils/removeAll arr1 (int-array idxs)))
                          (System/arraycopy arr2 0 joined (alength arr1) (alength arr2))
                          (System/arraycopy arr1 0 joined 0 (alength arr1))
                          {column joined})))
                 (into {}))
            (assoc :instrument (:instrument q1)
                   :period (:period q1))))))
  (cleanup
    ^Quotes [quotes]
    (map->Quotes (select-keys quotes [:instrument :period :time# :open#
                                      :high# :low# :close# :volume# :spread#
                                      :open-bid# :high-bid# :low-bid# :close-bid#
                                      :open-ask# :high-ask# :low-ask# :close-ask#])))
  ;(cleanup-hours
  ;  [quotes]
  ;  (let [^objects time# (:time# quotes)
  ;        idxs (->> (zipmap (range) time#)
  ;                  (map (fn [[idx date-time]] (when (within-forex-hours? date-time) idx)))
  ;                  (into [] (comp (remove nil?))))]
  ;    (ArrayUtils/removeAll time# (int-array idxs))))
  (fill-gaps ^Quotes
  [quotes]
    {:post [(empty? (filter nil? (:time# %)))]}
    (let [instrument (:instrument quotes)
          period (:period quotes)
          seconds (tim/period->seconds period)
          #^"[Lorg.joda.time.DateTime;" old-time# (:time# quotes)
          ^doubles old-open# (:open# quotes)
          ^doubles old-high# (:high# quotes)
          ^doubles old-low# (:low# quotes)
          ^doubles old-close# (:close# quotes)
          ^longs old-volume# (:volume# quotes)
          ^doubles old-open-bid# (:open-bid# quotes)
          ^doubles old-high-bid# (:high-bid# quotes)
          ^doubles old-low-bid# (:low-bid# quotes)
          ^doubles old-close-bid# (:close-bid# quotes)
          ^doubles old-open-ask# (:open-ask# quotes)
          ^doubles old-high-ask# (:high-ask# quotes)
          ^doubles old-low-ask# (:low-ask# quotes)
          ^doubles old-close-ask# (:close-ask# quotes)
          ^doubles old-spread# (:spread# quotes)
          old-size (alength old-time#)
          begin-time (aget old-time# (dec old-size))
          end-time (dt/plus (aget old-time# 0) (dt/seconds (period->seconds period)))
          #^"[Lorg.joda.time.DateTime;" new-time# (->> (dtp/periodic-seq begin-time end-time (dt/seconds seconds))
                                                       (filter #(within-trading-hours? instrument %))
                                                       (sort)
                                                       (reverse)
                                                       (into-array))
          new-size (alength new-time#)
          new-open# (double-array new-size)
          new-high# (double-array new-size)
          new-low# (double-array new-size)
          new-close# (double-array new-size)
          new-volume# (long-array new-size)
          new-open-bid# (double-array new-size)
          new-high-bid# (double-array new-size)
          new-low-bid# (double-array new-size)
          new-close-bid# (double-array new-size)
          new-open-ask# (double-array new-size)
          new-high-ask# (double-array new-size)
          new-low-ask# (double-array new-size)
          new-close-ask# (double-array new-size)
          new-spread# (double-array new-size)]
      (loop [i (dec new-size) j (dec old-size)]
        (when (>= i 0)
          (cond
            (= (aget new-time# i) (aget old-time# j))
            (do
              (aset new-open# i (aget old-open# j))
              (aset new-high# i (aget old-high# j))
              (aset new-low# i (aget old-low# j))
              (aset new-close# i (aget old-close# j))
              (aset new-volume# i (aget old-volume# j))
              (aset new-open-bid# i (aget old-open-bid# j))
              (aset new-high-bid# i (aget old-high-bid# j))
              (aset new-low-bid# i (aget old-low-bid# j))
              (aset new-close-bid# i (aget old-close-bid# j))
              (aset new-open-ask# i (aget old-open-ask# j))
              (aset new-high-ask# i (aget old-high-ask# j))
              (aset new-low-ask# i (aget old-low-ask# j))
              (aset new-close-ask# i (aget old-close-ask# j))
              (aset new-spread# i (aget old-spread# j))
              (recur (dec i) (dec j)))
            (dt/before? (aget new-time# i) (aget old-time# j))
            (do
              (aset new-open# i (aget new-close# (inc i)))
              (aset new-high# i (aget new-close# (inc i)))
              (aset new-low# i (aget new-close# (inc i)))
              (aset new-close# i (aget new-close# (inc i)))
              (aset new-volume# i 0)
              (aset new-open-bid# i (aget new-close-bid# (inc i)))
              (aset new-high-bid# i (aget new-close-bid# (inc i)))
              (aset new-low-bid# i (aget new-close-bid# (inc i)))
              (aset new-close-bid# i (aget new-close-bid# (inc i)))
              (aset new-open-ask# i (aget new-close-ask# (inc i)))
              (aset new-high-ask# i (aget new-close-ask# (inc i)))
              (aset new-low-ask# i (aget new-close-ask# (inc i)))
              (aset new-close-ask# i (aget new-close-ask# (inc i)))
              (aset new-spread# i (aget new-spread# (inc i)))
              (recur (dec i) j))
            (dt/after? (aget new-time# i) (aget old-time# j))
            (do
              (recur i (dec j))))))
      (->Quotes (:instrument quotes) (:period quotes) new-time#
                new-open# new-high# new-low# new-close# new-volume#
                new-open-bid# new-high-bid# new-low-bid# new-close-bid#
                new-open-ask# new-high-ask# new-low-ask# new-close-ask#
                new-spread#)))

  PQDataTime
  ;(period [quotes]
  ;  (let [seconds (->> quotes
  ;                     :time
  ;                     (partition 2)
  ;                     (map (fn [[t1 t2]] (dt/in-seconds (dt/interval t2 t1))))
  ;                     (frequencies)
  ;                     (sort-by second)
  ;                     (last)
  ;                     (first))]
  ;    (seconds->period seconds)))

  (last-candle-time ^DateTime [quotes]
    (aget ^objects (:time# quotes) 0))

  (next-candle-time ^DateTime [quotes]
    (dt/plus (last-candle-time quotes) (dt/seconds (tim/period->seconds (:period quotes))))))

