(ns machine.utils
  (:require [schema.core :as s]))


(s/defn oposite-side :- (s/enum :sell :buy)
  [side :- (s/enum :sell :buy)]
  (condp = side
    :buy :sell
    :sell :buy))


(s/defn split-instrument :- [s/Keyword]
  [instrument :- s/Keyword]
  (mapv keyword (clojure.string/split (name instrument) #"-")))


(defn roll-apply
  [f n coll]
  (map f (partition n 1 coll)))


(defn partition-between [pred? coll]
  (let [switch (reductions not= true (map pred? coll (rest coll)))]
    (map (partial map first) (partition-by second (map list coll switch)))))