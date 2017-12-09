(ns machine.core
  (:require [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as appenders]
            [mount.core :as mount]
            [machine.settings.io :refer [load-settings!]]
            [machine.db.core]
            [machine.router.core]
            [machine.oanda.core]
            [machine.oanda.workers]
            [machine.data-management.workers]
            [machine.time-management.workers]
            [machine.analyse-management.workers]
            [machine.trade-management.workers]
            [machine.strategy.fvb]
            [machine.strategy.vtr])
  (:gen-class))


(defn -main [& args]
  (load-settings!)
  (clojure.java.io/delete-file (clojure.java.io/as-file "./log.txt") true)
  (timbre/merge-config! {:appenders {:spit    (assoc (appenders/spit-appender {:fname "log.txt"})
                                                :min-level :info),
                                     :println {:min-level :info
                                               :async?    true}}})
  (mount/start)
  (while true
    (Thread/sleep 1000)))
