(ns machine.settings.core
  (:require [clojure.pprint :refer [pprint]]
            [mount.core :refer [defstate]]
            [schema.core :as s]
            [com.rpl.specter :as specter]
            [machine.db.core :refer [DB]]
            [machine.settings.schema :refer :all]
            [machine.settings.io :as io]))


(declare settings)
(defstate settings
          :start (io/load-settings!)
          :stop nil)


;(s/defn get-settings
;  ([]
;    (get @DB :settings))
;  ([id :- s/Num]
;    (first (filter #(== id (get-in % [:oanda :id])) (get-settings))))
;  ([id :- s/Num ks :- [s/Keyword]]
;    (get-in (first (filter #(== id (get-in % [:oanda :id])) (get-settings))) ks)))


;(s/defn add-settings!
;  [settings :- OandaAccountSchema]
;  (swap! DB update :settings conj settings))


;(s/defn dissoc-settings!
;  [id :- s/Num]
;  (swap! DB update :settings #(remove (fn [m] (== id (get-in m [:oanda :id]))) %)))


;(s/defn dissoc-in-settings!
;  [id :- s/Num ks :- [s/Keyword]]
;  (let [settings (get-settings id)
;        updated-settings (if-not (> (count ks) 1)
;                           (dissoc settings (first ks))
;                           (update-in settings (drop-last ks) dissoc (last ks)))]
;    (dissoc-settings! id)
;    (add-settings! updated-settings)))


;(s/defn update-settings!
;  ([id :- s/Num k :- s/Keyword f & args]
;    (let [m (get-settings id)]
;      (dissoc-settings! id)
;      (add-settings! (assoc m k (apply f (get m k) args))))))


;(s/defn update-in-settings!
;  ([id :- s/Num [k & ks] f & args]
;    (let [m (get-settings id)]
;      (dissoc-settings! id)
;      (if ks
;        (add-settings! (assoc m k (apply update-in (get m k) ks f args)))
;        (add-settings! (assoc m k (apply f (get m k) args)))))))


;(s/defn set-settings!
;  [id :- s/Num settings :- OandaAccountSchema]
;  (dissoc-settings! id)
;  (add-settings! settings))


;(s/defn assoc-settings!
;  [id :- s/Num k :- s/Keyword v]
;  (let [m (get-settings id)]
;    (dissoc-settings! id)
;    (add-settings! (assoc m k v))))


;(s/defn assoc-in-settings!
;  [id :- s/Num ks :- [s/Keyword] v]
;  (let [m (get-settings id)]
;    (dissoc-settings! id)
;    (add-settings! (assoc-in m ks v))))


(s/defn all-oanda-accounts
  []
  (mapv :oanda settings))


(s/defn all-instruments :- [s/Keyword]
  []
  (distinct
    (specter/select* [specter/ALL :strategy specter/ALL specter/ALL :instruments specter/ALL distinct]
                    settings)))


(s/defn all-periods :- [s/Keyword]
  []
  (distinct
    (specter/select* [specter/ALL :strategy specter/ALL specter/ALL :periods specter/ALL]
                    settings)))