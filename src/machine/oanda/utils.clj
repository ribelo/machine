(ns machine.oanda.utils
  (:refer-clojure :exclude [format])
  (:require [schema.core :as s]
            [taoensso.encore :as encore]
            [camel-snake-kebab.core :refer :all]
            [clj-time.coerce :as dtc]
            [machine.utils :refer :all]))


(def from-oanda
  {:account-currency ->kebab-case-keyword
   :pip              #(if (string? %) (read-string %) %)
   :instrument       #(->kebab-case-keyword % :separator \_)
   :period           #(keyword (clojure.string/lower-case %))
   :time             #(if (string? %)
                       (-> % (read-string) (* 0.001) (long) (dtc/from-long))
                       %)
   :expiry           #(if (string? %)
                       (-> % (read-string) (* 0.001) (long) (dtc/from-long))
                       %)
   :side             ->kebab-case-keyword
   :type             ->kebab-case-keyword
   :granularity      #(-> % clojure.string/lower-case keyword)
   :status           #(keyword (clojure.string/lower-case %))})


(def to-oanda
  {:fields      (fn [fields]
                  (not-empty (clojure.string/join
                               "," (mapv #(-> % name ->camelCase) fields))))
   :instruments (fn [instruments]
                  (not-empty (clojure.string/join
                               "," (mapv (fn [k] (->SCREAMING_SNAKE_CASE (name k) :separator \-))
                                         instruments))))
   :instrument  (fn [k] (->SCREAMING_SNAKE_CASE (name k) :separator \-))
   :period      (fn [period] (clojure.string/upper-case (name period)))
   :expiry      #(-> % dtc/to-string)
   :side        #(clojure.string/lower-case (name %))
   :type        #(clojure.string/lower-case (name %))})


(def translation                                            ;FIXME Cleanup
  (->> {:period        :granularity
        :open-bid      :open
        :high-bid      :high
        :low-bid       :low
        :close-bid     :close
        :complete      :complete?
        :pl            :profit
        :unrealized-pl :unrealized-profit
        :realized-pl   :realized-profit
        :margin-avail  :margin-available
        :halted        :halted?}
       (map (fn [[k v]]
              {k v
               v k}))
       (into {})))


(defn parse [resp dir]
  (when resp
    (let [f #(->> %
                  (clojure.walk/prewalk
                    (fn [x] (if (map? x)
                              (clojure.set/rename-keys
                                (->> x
                                     (into [])
                                     (map (fn [[k v]]
                                            (if-let [f (get dir k)]
                                              [k (f v)]
                                              [k v])))
                                     (into {}))
                                translation)
                              x)))
                  (into {}))]
      (cond
        (map? resp) (f resp)
        (sequential? resp) (mapv f resp)))))

(defn parse-from-oanda [resp]
  (when resp
    (let [f #(->> %
                  (clojure.walk/prewalk
                    (fn [x] (if (map? x)
                              (clojure.set/rename-keys
                                (->> x
                                     (into [])
                                     (map (fn [[k v]]
                                            (if-let [f (get from-oanda k)]
                                              [(->kebab-case-keyword k) (f v)]
                                              [(->kebab-case-keyword k) v])))
                                     (into {}))
                                {:granularity   :period
                                 :complete      :complete?
                                 :pl            :profit
                                 :unrealized-pl :unrealized-profit
                                 :realized-pl   :realized-profit
                                 :margin-avail  :margin-available
                                 :halted        :halted?})
                              x)))
                  (into {}))]
      (cond
        (map? resp) (f resp)
        (sequential? resp) (mapv f resp)))))


(defn parse-to-oanda [resp]
  (when resp
    (let [f #(->> %
                  (clojure.walk/prewalk
                    (fn [x] (if (map? x)
                              (clojure.set/rename-keys
                                (->> x
                                     (into [])
                                     (map (fn [[k v]]
                                            (if-let [f (get to-oanda k)]
                                              [(->camelCaseKeyword k) (f v)]
                                              [(->camelCaseKeyword k) v])))
                                     (into {}))
                                {:period            :granularity
                                 :complete?         :complete
                                 :profit            :pl
                                 :unrealized-profit :unrealized-pl
                                 :realized-profit   :realized-pl
                                 :margin-available  :margin-avail
                                 :halted?           :halted})
                              x)))
                  (into {}))]
      (cond
        (map? resp) (f resp)
        (sequential? resp) (mapv f resp)))))


(s/defn precision :- double
  [x :- double p :- double]
  (* (Math/round ^double (/ x p)) p))