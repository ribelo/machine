(ns machine.settings.io
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [schema.core :as s]
            [machine.db.core :refer [DB]]
            [machine.settings.schema :refer [SettingsSchema]]))


(s/defn settings-file-exists? :- s/Bool
  []
  (.exists (io/as-file "./config.edn")))


(s/defn create-settings-file!
  [settings]
  (pprint settings (clojure.java.io/writer "./config.edn")))


(def default-settings
  [{:oanda    {:id      nil
               :token   nil
               :type    nil
               :headers {}}
    :money    {:max-risk           nil
               :max-dd             nil
               :min-margin-level   nil
               :max-spread-percent nil
               :max-open-trades    nil}
    :strategy {}}])


(s/defn load-settings!
  ([path :- s/Str]
    (if (settings-file-exists?)
      (->> (slurp path)
           (clojure.edn/read-string)
           (s/validate SettingsSchema))
      (do (create-settings-file! default-settings)
          (throw (Exception. "config file does not exist!")))))
  ([]
    (load-settings! "./config.edn")))