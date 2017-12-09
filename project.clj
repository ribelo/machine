(defproject machine "0.9.6-SNAPSHOT"
  :description "FIXME: write description"
  :min-lein-version "2.6.1"
  :url "http://example.com/FIXME"
  :main machine.core
  :jvm-opts ["-server"]
  :resource-paths []
  :plugins [[lein-gorilla "0.3.5"]]
  :clean-targets ^{:protect false} [:target-path
                                    [:cljsbuild :builds :app :compiler :output-dir]
                                    [:cljsbuild :builds :app :compiler :output-to]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.taoensso/encore "2.92.0"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [org.apache.commons/commons-lang3 "3.6"]
                 [mount "0.1.11" :exclusions [ch.qos.logback/logback-classic]]
                 [prismatic/schema "1.1.6"]
                 [prismatic/plumbing "0.5.4"]
                 [cheshire "5.8.0"]
                 [camel-snake-kebab "0.4.0"]
                 [com.taoensso/timbre "4.10.0"]
                 ;[clj-http "2.1.0"]
                 [http-kit "2.2.0"]
                 [clj-time "0.12.0"]
                 [criterium "0.4.4"]
                 [com.rpl/specter "1.0.3"]]
  :profiles {:dev     {:dependencies [[incanter/incanter-core "1.9.1"]]}
             :uberjar {:uberjar-name "machine-standalone.jar"
                       :omit-source  true
                       :test         [:project/test :profiles/test]}})
