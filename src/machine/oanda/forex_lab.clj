(ns ^{:skip-aot true}
  machine.oanda.forex-lab
  (:require [schema.core :as s]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clj-time.coerce :as dtc]
            [camel-snake-kebab.core :refer :all]
            [taoensso.timbre :as timbre]
            [taoensso.encore :refer [dissoc-in path reset-in! assoc-some round]]
            [machine.time-management.core :as tim]
            [machine.oanda.core :refer [oanda-config]]
            [machine.oanda.utils :refer :all]
            [machine.oanda.schema :refer :all]
            [machine.utils :refer [period->seconds]]
            [machine.db.core :as db]))



(s/defn calendar :- s/Any                                   ;TODO
  [instrument :- s/Keyword period :- s/Keyword]
  (let [{:keys [url token headers cm]} oanda-config]
    (if-let [memoized (db/cache-out! [:oanda :calendar instrument period])]
      memoized
      (when-let [resp (http/get (path url "labs" "v1" "calendar")
                                {:connection-manager   cm
                                 :throw-exceptions     false
                                 :ignore-unknown-host? true
                                 :headers              headers
                                 :oauth-token          token
                                 :query-params         {:instrument instrument
                                                        :period     (tim/period->seconds period)}})]
        (if (= 200 (:status resp))
          (let [result (-> resp
                           :body
                           (json/parse-string ->kebab-case-keyword)
                           (parse-from-oanda)
                           (->> (map (fn [m]
                                       (assoc-some m
                                         :forecast (let [v (:forecast m)]
                                                     (when v (read-string v)))
                                         :currency (let [v (:currency m)]
                                                     (when v (keyword (clojure.string/lower-case v))))
                                         :previous (let [v (:previous m)]
                                                     (when v (read-string v)))
                                         :market (let [v (:market m)]
                                                   (when v (read-string v)))
                                         :actual (let [v (:actual m)]
                                                   (when v (read-string v)))
                                         :timestamp (let [v (:timestamp m)]
                                                      (when v (dtc/from-long (* 1000 v)))))))))]
            ;(timbre/debug "[accounts-info] - get info successful")
            (db/cache-in! [:oanda :calendar instrument period] result)
            result)
          (timbre/error "[accounts-info] - error" (:status resp)
                        (json/parse-string (:body resp) ->kebab-case-keyword)))))))


(s/defn spreads :- s/Any
  [instrument :- s/Keyword period :- s/Keyword]
  (let [{:keys [url token headers cm]} oanda-config]
    (if-let [memoized (db/cache-out! [:oanda :spreads instrument period])]
      memoized
      (when-let [resp (http/get (path url "labs" "v1" "spreads")
                                {:connection-manager   cm
                                 :throw-exceptions     false
                                 :ignore-unknown-host? true
                                 :headers              headers
                                 :oauth-token          token
                                 :query-params         {:instrument (->SCREAMING_SNAKE_CASE (name instrument) :separator \-)
                                                        :period     (tim/period->seconds period)}})]
        (if (= 200 (:status resp))
          (let [result (-> resp
                           :body
                           (json/parse-string ->kebab-case-keyword)
                           (parse-from-oanda))]
            ;(timbre/debug "[accounts-info] - get info successful")
            (db/cache-in! [:oanda :spreads instrument period] result)
            result)
          (timbre/error "[accounts-info] - error" (:status resp)
                        (json/parse-string (:body resp) ->kebab-case-keyword)))))))
(http/get (path (:url oanda-config) "labs" "v1" "spreads")
          {:oauth-token  (:token oanda-config)
           :debug        true
           :query-params (parse-to-oanda
                           {:instrument :eur-usd
                            :period     :d})})
((:avg (spreads :eur-usd :w)))

(s/defn order-book :- s/Any
  [instrument :- s/Keyword period :- s/Keyword]
  (let [{:keys [url token headers cm]} oanda-config]
    (println {:instrument instrument
              :period     (tim/period->seconds period)})
    (if-let [memoized (db/cache-out! [:oanda :order-book instrument period])]
      memoized
      (when-let [resp (http/get (path url "labs" "v1" "orderbook_data")
                                {:connection-manager   cm
                                 :throw-exceptions     false
                                 :ignore-unknown-host? true
                                 :headers              headers
                                 :oauth-token          token
                                 :query-params         {:instrument instrument
                                                        :period     (tim/period->seconds period)}})]
        (if (= 200 (:status resp))
          (let [result (-> resp
                           :body
                           (json/parse-string ->kebab-case-keyword)
                           (parse-from-oanda))]
            ;(timbre/debug "[accounts-info] - get info successful")
            (db/cache-in! [:oanda :order-book instrument period] result)
            result)
          (timbre/error "[accounts-info] - error" (:status resp)
                        (json/parse-string (:body resp) ->kebab-case-keyword)))))))

(order-book :xau-usd :d)