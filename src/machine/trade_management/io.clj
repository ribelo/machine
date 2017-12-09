(ns machine.trade-management.io
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [schema.core :as s]
            [mount.core :refer [defstate]]
            [taoensso.timbre :as timbre])
  (:import [clojure.lang PersistentArrayMap]))

(s/defn trades-history-exists?
  []
  (.exists (io/as-file "./trades-history.edn")))


(s/defn save-history-file!
  [trades-history :- [{s/Keyword s/Any}]]
  (pprint trades-history (clojure.java.io/writer "./trades-history.edn")))


(s/defn add-trade-to-history!
  [trade :- {s/Keyword s/Any}]
  (if (trades-history-exists?)
    (let [trades-history (-> "./trades-history.edn"
                             (slurp)
                             (clojure.edn/read-string)
                             (vec))]
      (save-history-file! (conj trades-history trade)))
    (save-history-file! [trade])))