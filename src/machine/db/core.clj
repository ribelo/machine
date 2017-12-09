(ns machine.db.core
  (:require [mount.core :refer [defstate]]
            [schema.core :as s]
            [clj-time.core :as dt]
            [taoensso.timbre :as timbre]
            [taoensso.encore :refer [dissoc-in path assoc-some round reset-in! when-lets]]
            [machine.quotes.core :as q]
            [machine.utils :refer :all]
            [machine.settings.schema :refer :all])
  (:import [machine.quotes.core Quotes]))


(s/set-fn-validation! true)

(declare DB)
(defstate DB
          :start (atom {})
          :stop (reset! DB {}))


(s/defn cache-in!
  ([ks :- [(s/either s/Keyword s/Num)] v :- s/Any]
    (cache-in! ks v nil))
  ([ks :- [(s/either s/Keyword s/Num)] v :- s/Any ttl :- (s/maybe s/Num)]
    (doto DB
      (reset-in! (conj (into [:cache] ks) :value) v)
      (reset-in! (conj (into [:cache] ks) :time)
                 (when (or (nil? ttl) (pos? ttl))
                   (dt/plus (dt/now) (dt/millis (or ttl 0))))))))


(s/defn cache-out!
  [ks :- [(s/either s/Keyword s/Num)]]
  (when (and (get-in @DB (conj (into [:cache] ks) :time))
             (dt/before? (dt/now) (get-in @DB (conj (into [:cache] ks) :time))))
    (get-in @DB (conj (into [:cache] ks) :value))))


(s/defn get-quotes :- (s/maybe Quotes)
  [instrument :- s/Keyword period :- s/Keyword]
  (get-in @DB [:quotes instrument period]))


(s/defn get-column :- (s/maybe (s/cond-pre doubles longs booleans))
  [instrument :- s/Keyword period :- s/Keyword column :- s/Keyword]
  (when-let [data (get-in @DB [:quotes instrument period])]
    (q/get-column data column)))


(s/defn get-row :- (s/maybe {s/Keyword s/Any})
  [instrument :- s/Keyword period :- s/Keyword row :- s/Num]
  (when-let [data (get-in @DB [:quotes instrument period])]
    (q/get-row data row)))


(s/defn get-value :- s/Any
  [instrument :- s/Keyword period :- s/Keyword row :- s/Num column :- s/Keyword]
  (when-let [data (get-in @DB [:quotes instrument period])]
    (q/get data row column)))


(s/defn save-quotes!
  [instrument :- s/Keyword period :- s/Keyword quotes :- Quotes]
  (when (swap! DB assoc-in [:quotes instrument period] quotes)
    (timbre/debug "[save-quotes!] -" [instrument period] "- save quotes successful")))


(s/defn remove-quotes!
  [instrument :- s/Keyword period :- s/Keyword]
  (when (swap! DB assoc-in [:quotes instrument period] nil)
    (timbre/debug "[remove-quotes!] -" [instrument period] "- remove quotes successful")))


(s/defn stored-instruments
  []
  (vec (keys (:quotes @DB))))


(s/defn stored-periods [instrument]
  (vec (keys (get-in @DB [:quotes instrument]))))


(s/defn stored-quotes
  []
  (->> @DB
       :quotes
       (mapv (fn [[instrument data]]
               {:instrument instrument
                :periods    (vec (keys data))}))))








