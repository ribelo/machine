(ns machine.oanda.core
  (:require [clojure.core.async :refer [go go-loop >! <! <!! thread timeout]]
            [clojure.string :refer [join]]
            [mount.core :refer [defstate]]
            [schema.core :as s]
    ;[clj-http.client :as http]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clj-time.core :as dt]
            [clj-time.coerce :as dtc]
            [camel-snake-kebab.core :refer :all]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [taoensso.timbre :as timbre]
            [taoensso.encore :refer [dissoc-in path reset-in! assoc-some round]]
            [plumbing.core]
            [machine.oanda.utils :refer :all]
            [machine.utils :refer :all]
            [machine.db.core :as db :refer [DB]]
            [machine.oanda.schema :refer :all]
            [machine.time-management.core :as tim]
            [machine.settings.core :as settings])
  (:import [org.joda.time DateTime]
           [org.apache.commons.math3.util FastMath]))



(defrecord OandaAccount
  [account-id
   account-type
   token
   headers
   url
   stream])


(s/defn create-oanda-account :- OandaAccount
  ([{:keys [account-type] :as m} :- {:account-id s/Num :account-type s/Keyword
                                     :token      s/Str :headers {s/Str s/Str}}]
    (map->OandaAccount
      (merge m {:url    (case account-type
                          :fxtrade "https://api-fxtrade.oanda.com"
                          :fxpractice "https://api-fxpractice.oanda.com"
                          :sandbox "http://api-sandbox.oanda.com")
                :stream (case account-type
                          :fxtrade "https://stream-fxtrade.oanda.com"
                          :fxpractice "https://stream-fxpractice.oanda.com"
                          :sandbox "http://stream-sandbox.oanda.com")})))
  ([account-id :- s/Num account-type :- s/Keyword token :- s/Str headers :- {s/Keyword s/Str}]
    (create-oanda-account {:account-id   account-id
                           :account-type account-type
                           :token        token
                           :headers      headers})))


(declare oanda-accounts)
(defstate oanda-accounts
          :start (mapv (fn [{:keys [oanda money strategy]}]
                         (assoc (create-oanda-account oanda) :money money :strategy strategy))
                       settings/settings)
          :stop [])

(s/defn accounts-info :- (s/maybe AccountsResponse)
  [{:keys [account-id url token headers]} :- OandaAccount]
  (if-let [memoized (db/cache-out! [:oanda account-id :accounts-info])]
    memoized
    (when-let [resp @(http/get (path url "v1" "accounts")
                               {:throw-exceptions false
                                :headers          headers
                                :oauth-token      token})]
      (if (= 200 (:status resp))
        (let [result (-> resp
                         :body
                         (json/parse-string ->kebab-case-keyword)
                         (parse-from-oanda)
                         :accounts)]
          (timbre/debug "[accounts-info] -" [account-id] "- get info successful")
          (db/cache-in! [:oanda account-id :account-info] result)
          result)
        (timbre/error "[accounts-info] -" [account-id] "- error" (:status resp)
                      (json/parse-string (:body resp) ->kebab-case-keyword))))))


(s/defn account-info :- (s/maybe AccountResponse)
  ([{:keys [account-id url token headers]} :- OandaAccount]
    (when-let [resp @(http/get (path url "v1" "accounts" account-id)
                               {:throw-exceptions false
                                :headers          headers
                                :oauth-token      token})]

      (if (= 200 (:status resp))
        (let [result (-> resp
                         :body
                         (json/parse-string ->kebab-case-keyword)
                         (parse-from-oanda))]
          (timbre/debug "[account-info] -" [account-id] "- get info sucessful")
          (db/cache-in! [:oanda account-id :account-info] result 1000)
          result)
        (timbre/error "[account-info] -" [account-id] "- error" (:status resp)
                      (json/parse-string (:body resp) ->kebab-case-keyword))))))

(s/defn account-name :- (s/maybe (:account-name AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :account-name))


(s/defn account-currency :- (s/maybe (:account-currency AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :account-currency))


(s/defn margin-available :- (s/maybe (:margin-available AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :margin-available))


(s/defn margin-used :- (s/maybe (:margin-used AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :margin-used))


(s/defn margin-rate :- (s/maybe (:margin-rate AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :margin-rate))


(s/defn balance :- (s/maybe (:balance AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :balance))


(s/defn open-trades :- (s/maybe (:open-trades AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :open-trades))


(s/defn unrealized-profit :- (s/maybe (:unrealized-profit AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :unrealized-profit))


(s/defn realized-profit :- (s/maybe (:realized-profit AccountResponse))
  [{:keys [account-id] :as account} :- OandaAccount]
  (get (or (db/cache-out! [:oanda account-id :account-info])
           (account-info account)) :realized-profit))


(s/defn orders-info :- (s/maybe OrdersResponse)
  [{:keys [account-id url token headers]} :- OandaAccount]
  (when-let [resp @(http/get (path url "v1" "accounts" account-id "orders")
                             {:throw-exceptions false
                              :headers          headers
                              :oauth-token      token})]
    (if (= 200 (:status resp))
      (let [result (-> resp
                       :body
                       (json/parse-string ->kebab-case-keyword)
                       (parse-from-oanda)
                       :orders)]
        (timbre/debug "[orders-info] -" [account-id] "- get info sucessful")
        result)
      (timbre/error "[orders-info] -" [account-id] "- error" (:status resp)
                    (json/parse-string (:body resp) ->kebab-case-keyword)))))


(s/defn order-info :- (s/maybe OrderSchema)
  [{:keys [account-id url token headers]} :- OandaAccount order-id :- s/Num]
  (when-let [resp @(http/get (path url "v1" "accounts" account-id "orders" order-id)
                             {:throw-exceptions false
                              :headers          headers
                              :oauth-token      token})]
    (if (= 200 (:status resp))
      (let [result (-> resp
                       :body
                       (json/parse-string ->kebab-case-keyword)
                       (parse-from-oanda))]
        (timbre/debug "[order-info] - get" order-id "info sucessful")
        result)
      (timbre/error "[order-info] error" (:status resp)
                    (json/parse-string (:body resp) ->kebab-case-keyword)))))


(s/defn trades-info :- (s/maybe TradesResponse)
  [{:keys [account-id url token headers]} :- OandaAccount]
  (when-let [resp @(http/get (path url "v1" "accounts" account-id "trades")
                             {:throw-exceptions false
                              :headers          headers
                              :oauth-token      token})]
    (if (= 200 (:status resp))
      (let [result (-> resp
                       :body
                       (json/parse-string ->kebab-case-keyword)
                       (parse-from-oanda)
                       :trades)]
        (timbre/debug "[trades-info] -" [account-id] "- get info sucessful")
        result)
      (timbre/error "[trades-info] -" [account-id] "- error" (:status resp)
                    (json/parse-string (:body resp) ->kebab-case-keyword)))))


(s/defn trade-info :- (s/maybe TradeSchema)
  [{:keys [account-id url token headers]} :- OandaAccount trade-id :- s/Num]
  (when-let [resp @(http/get (path url "v1" "accounts" account-id "trades" trade-id)
                             {:throw-exceptions false
                              :headers          headers
                              :oauth-token      token})]
    (if (= 200 (:status resp))
      (let [result (-> resp
                       :body
                       (json/parse-string ->kebab-case-keyword)
                       (parse-from-oanda))]
        (timbre/debug "[trade-info] - get" trade-id "info sucessful")
        result)
      (timbre/error "[trade-info] -" [account-id] "- error" (:status resp)
                    (json/parse-string (:body resp) ->kebab-case-keyword)))))


(s/defn instruments-info :- (s/maybe InstrumentsResponse)
  ([{:keys [account-id url token headers]} :- OandaAccount]
    (if-let [memoized (db/cache-out! [:oanda account-id :instruments-info])]
      memoized
      (let [fields [:instrument :display-name :pip
                    :max-trade-units :max-trailing-stop
                    :min-trailing-stop :margin-rate :halted]
            query (-> {}
                      (assoc-some :fields fields)
                      (assoc :accountId account-id)
                      (parse-to-oanda))]
        (when-let [resp @(http/get (path url "v1" "instruments")
                                   {:throw-exceptions false
                                    :headers          headers
                                    :oauth-token      token
                                    :query-params     query})]
          (if (= 200 (:status resp))
            (let [result (-> resp
                             :body
                             (json/parse-string ->kebab-case-keyword)
                             (parse-from-oanda)
                             :instruments)]
              (timbre/debug "[instruments-info] -" [account-id] "- get info sucessful")
              (db/cache-in! [:oanda account-id :instruments-info] result 5000)
              result)
            (timbre/error "[instruments-info] -" [account-id] "- error" (:status resp)
                          (json/parse-string (:body resp) ->kebab-case-keyword)))))))
  ([account :- OandaAccount instruments :- [s/Keyword]]
    (->> (instruments-info account)
         (filterv #((set instruments) (:instrument %))))))


(s/defn instrument-info :- (s/maybe InstrumentSchema)
  [account :- OandaAccount instrument :- s/Keyword]
  (first (instruments-info account [instrument])))


(s/defn instrument-display-name :- (s/maybe (:display-name InstrumentSchema))
  [account :- OandaAccount instrument :- s/Keyword]
  (get (instrument-info account instrument) :display-name))


(s/defn instrument-pip :- (s/maybe (:pip InstrumentSchema))
  [account :- OandaAccount instrument :- s/Keyword]
  (get (instrument-info account instrument) :pip))


(s/defn instrument-max-trade-units :- (s/maybe (:max-trade-units InstrumentSchema))
  [account :- OandaAccount instrument :- s/Keyword]
  (get (instrument-info account instrument) :max-trade-units))


(s/defn instrument-max-trailing-stop :- (s/maybe double)
  [account :- OandaAccount instrument :- s/Keyword]
  (get (instrument-info account instrument) :max-trailing-stop))


(s/defn instrument-min-trailing-stop :- (s/maybe double)
  [account :- OandaAccount instrument :- s/Keyword]
  (get (instrument-info account instrument) :min-trailing-stop))


(s/defn instrument-margin-rate :- (s/maybe double)
  [account :- OandaAccount instrument :- s/Keyword]
  (get (instrument-info account instrument) :margin-rate))


(s/defn instrument-halted? :- (s/maybe s/Bool)
  [account :- OandaAccount instrument :- s/Keyword]
  (get (instrument-info account instrument) :halted?))


(s/defn instruments-available :- (s/maybe [s/Keyword])
  [account :- OandaAccount]
  (mapv :instrument (instruments-info account)))


(s/defn instruments-active :- (s/maybe [s/Keyword])
  ([account :- OandaAccount]
    (->> (instruments-info account)
         (remove :halted?)
         (mapv :instrument)))
  ([account :- OandaAccount instruments :- [s/Keyword]]
    (filterv (set instruments) (instruments-active account))))


(s/defn prices-info :- (s/maybe PricesResponse)
  ([{:keys [account-id url token headers] :as account} :- OandaAccount]
    (if-let [memoized (db/cache-out! [:oanda account-id :prices-info])]
      memoized
      (let [query (-> {}
                      (assoc :accountId account-id
                             :instruments (instruments-available account))
                      (parse-to-oanda))]
        (when-let [resp @(http/get (path url "v1" "prices")
                                   {:throw-exceptions false
                                    :headers          headers
                                    :oauth-token      token
                                    :query-params     query})]
          (if (= 200 (:status resp))
            (let [result (-> resp
                             :body
                             (json/parse-string ->kebab-case-keyword)
                             (parse-from-oanda)
                             :prices
                             (->> (mapv (fn [{:keys [status] :as info}]
                                          (-> info
                                              (clojure.set/rename-keys {:status :halted?})
                                              (assoc :halted? (if status true false)))))))]
              (timbre/debug "[prices-info] -" [account-id] "- get info sucessful")
              (db/cache-in! [:oanda account-id :prices-info] result 1000)
              result)
            (timbre/error "[prices-info] -" [account-id] "- error" (:status resp)
                          (json/parse-string (:body resp) ->kebab-case-keyword)))))))
  ([account :- OandaAccount instruments :- [s/Keyword]]
    (->> (prices-info account)
         (filterv #((set instruments) (:instrument %))))))


(s/defn price-info :- (s/maybe PriceSchema)
  [account :- OandaAccount instrument :- s/Keyword]
  (->> (prices-info account)
       (filterv #(= instrument (:instrument %)))
       (first)))


(s/defn price-bid :- (s/maybe (:bid PriceSchema))
  [account :- OandaAccount instrument :- s/Keyword]
  (:bid (price-info account instrument)))


(s/defn price-ask :- (s/maybe (:ask PriceSchema))
  [account :- OandaAccount instrument :- s/Keyword]
  (:ask (price-info account instrument)))


(s/defn instrument-time :- (s/maybe (:time PriceSchema))
  [account :- OandaAccount instrument :- s/Keyword]
  (:time (price-info account instrument)))


(s/defn instrument-spread :- (s/maybe s/Num)
  [account :- OandaAccount instrument :- s/Keyword]
  (- (price-ask account instrument) (price-bid account instrument)))


(s/defn instrument-base-home-exchange-rate :- (s/maybe s/Num)
  [account :- OandaAccount instrument :- s/Keyword]
  (let [account-currency (account-currency account)]
    (if (<= (count (name instrument)) 7)
      (let [instrument-base (first (clojure.string/split (name instrument) #"-"))
            quote-home (keyword (str (name instrument-base) "-" (name account-currency)))
            home-quote (keyword (str (name account-currency) "-" (name instrument-base)))
            quote-home-price (price-bid account quote-home)
            home-quote-price (price-bid account home-quote)]
        (taoensso.encore/round
          (if quote-home-price
            quote-home-price
            (/ 1 (or home-quote-price 1)))
          :round 5))
      (let [instrument-quote (second (clojure.string/split (name instrument) #"-"))]
        (if (= instrument-quote (name account-currency))
          (price-bid account instrument)
          (let [exchange-rate (or (instrument-base-home-exchange-rate account
                                                                      (keyword (str instrument-quote "-" (name account-currency))))
                                  (instrument-base-home-exchange-rate account
                                                                      (keyword (str (name account-currency) "-" instrument-quote))))]
            (* exchange-rate (price-bid account instrument))))))))


(s/defn instrument-home-base-exchange-rate :- (s/maybe s/Num)
  [account :- OandaAccount instrument :- s/Keyword]
  (let [account-currency (account-currency account)]
    (if (<= (count (name instrument)) 7)
      (let [instrument-base (second (clojure.string/split (name instrument) #"-"))
            quote-home (keyword (str (name instrument-base) "-" (name account-currency)))
            home-quote (keyword (str (name account-currency) "-" (name instrument-base)))
            quote-home-price (price-bid account quote-home)
            home-quote-price (price-bid account home-quote)]
        (taoensso.encore/round
          (if quote-home-price
            quote-home-price
            (/ 1 (or home-quote-price 1)))
          :round 5))
      (let [instrument-quote (second (clojure.string/split (name instrument) #"-"))]
        (if (= instrument-quote (name account-currency))
          1.0
          (let [exchange-rate (or (instrument-home-base-exchange-rate account
                                                                      (keyword (str (name account-currency) "-" instrument-quote)))
                                  (instrument-home-base-exchange-rate account
                                                                      (keyword (str instrument-quote "-" (name account-currency)))))]
            exchange-rate))))))


(s/defn instrument-units-available :- (s/maybe s/Num)
  ([account :- OandaAccount instrument :- s/Keyword eq :- s/Num]
    (long (Math/floor (/ (/ (FastMath/floor eq)
                            (margin-rate account))
                         (instrument-base-home-exchange-rate account instrument)))))
  ([account :- OandaAccount instrument :- s/Keyword]
    (instrument-units-available account instrument (margin-available account))))


(s/defn instrument-tick-value :- (s/maybe s/Num)
  [account :- OandaAccount instrument :- s/Keyword]
  (* (instrument-home-base-exchange-rate account instrument)
     (instrument-pip account instrument)))


(s/defn instrument-pip-value :- (s/maybe s/Num)
  [account :- OandaAccount instrument :- s/Keyword units :- s/Num]
  (* (instrument-tick-value account instrument) units))


;(s/defn instruments-invested :- (s/maybe (:display-name InstrumentInfoSchema))
;  [oanda]
;  (when-let [trades-info (trades-infooanda)]
;    ()))


;(s/defn instrument-invested? :- (s/maybe (:display-name InstrumentInfoSchema))
;  [instrument :- s/Keyword]
;  (when-let [trades-info ()]))


(s/defn quotes-history                                      ; :- (s/maybe Quotes)
  ([{:keys [account-id url token headers]} :- OandaAccount
    {:keys [instrument period] :as query} :- QutesHistoryQuery]
    (let [query (parse-to-oanda query)]
      (when-let [resp @(http/get (path url "v1" "candles")
                                 {:throw-exceptions false
                                  :headers          headers
                                  :oauth-token      token
                                  :query-params     query})]
        (condp = (:status resp)
          200
          (do
            (timbre/debug "[quotes-history] - " [account-id instrument period] "get history sucessful")
            (-> resp
                :body
                (json/parse-string ->kebab-case-keyword)
                :candles
                (->> (map (fn [{:keys [time open-bid high-bid low-bid
                                       close-bid open-ask high-ask low-ask
                                       close-ask volume complete]}]
                            (when complete
                              {:instrument instrument
                               :period     period
                               :time#      (dtc/from-long (/ (Long/parseLong time) 1000))
                               :open-bid#  (double open-bid)
                               :high-bid#  (double high-bid)
                               :low-bid#   (double low-bid)
                               :close-bid# (double close-bid)
                               :open-ask#  (double open-ask)
                               :high-ask#  (double high-ask)
                               :low-ask#   (double low-ask)
                               :close-ask# (double close-ask)
                               :open#      (double (* (+ open-bid open-ask) 0.5))
                               :high#      (double (* (+ high-bid high-ask) 0.5))
                               :low#       (double (* (+ low-bid low-ask) 0.5))
                               :close#     (double (* (+ close-bid close-ask) 0.5))
                               :spread#    (double (- close-ask close-bid))
                               :volume#    volume})))
                     (remove nil?)
                     (reverse)
                     (into [])
                     (not-empty))))
          204
          (timbre/debug "no content")
          (timbre/error "[quotes-history] -" [account-id] "- error" (:status resp)
                        (json/parse-string (:body resp) ->kebab-case-keyword))))))
  ([account :- OandaAccount instrument :- s/Keyword period :- s/Keyword candles-count :- s/Num]
    (let [query {:instrument        instrument
                 :period            period
                 :count             candles-count
                 :candleFormat      "bidask"
                 :dailyAlignment    0
                 :weeklyAlignment   "Monday"
                 :alignmentTimezone "UTC"}]
      (quotes-history account query)))
  ([account :- OandaAccount instrument :- s/Keyword period :- s/Keyword start :- DateTime end :- DateTime]
    (let [ps (tim/period->seconds period)
          in-seconds (dt/in-seconds (dt/interval start end))
          candles-count (/ in-seconds ps)]
      (if (> candles-count 4096)
        (let [middle (tim/floor-time (dt/plus start (dt/seconds (/ in-seconds 2))) ps)
              first-chunk (quotes-history account instrument period start middle)
              second-chunk (quotes-history account instrument period (dt/plus middle (dt/seconds ps)) end)]
          (if-not (nil? second-chunk)
            (->> (concat first-chunk second-chunk)
                 (sort-by :time#)
                 (reverse)
                 (vec))
            (->> first-chunk
                 (sort-by :time#)
                 (reverse)
                 (vec))))
        (let [query {:instrument        instrument
                     :period            period
                     :start             (long (/ (dtc/to-long start) 1000))
                     :end               (long (/ (dtc/to-long end) 1000))
                     :includeFirst      false
                     :candleFormat      "bidask"
                     :dailyAlignment    0
                     :weeklyAlignment   "Monday"
                     :alignmentTimezone "UTC"}]
          (quotes-history account query))))))


(s/defn transactions-history :- (s/maybe TransactionsResponse)
  ([account :- OandaAccount]
    (transactions-history account nil))
  ([{:keys [account-id url token headers] :as account} :- OandaAccount count :- (s/maybe s/Num)]
    (let [parse-transactions
          (fn [transactions]
            (->> transactions
                 (map (fn [close-trade]
                        (when (#{:trade-close :take-profit-filled :stop-loss-filled} (:type close-trade))
                          (let [pip (instrument-pip account (:instrument close-trade))]
                            (when-let [open-trade (->> transactions
                                                       (filter #(and (#{:market-order-create
                                                                        :market-if-touched-order-create}
                                                                       (:type %))
                                                                     (= (:trade-id close-trade) (:id %))))
                                                       (first))]
                              (when-let [profit (condp = (:side close-trade)
                                                  :buy (/ (- (:price open-trade) (:price close-trade)) pip)
                                                  :sell (/ (- (:price close-trade) (:price open-trade)) pip)
                                                  nil)]
                                (assoc close-trade :pips-profit profit)))))))
                 (into [] (comp (remove nil?)))))]
      (when-let [resp @(http/get (path url "v1" "accounts" account-id "transactions")
                                 {:throw-exceptions false
                                  :headers          headers
                                  :oauth-token      token
                                  :query-params     (if count
                                                      {:count count}
                                                      {})})]
        (if (= 200 (:status resp))
          (let [result (-> resp
                           :body
                           (json/parse-string ->kebab-case-keyword)
                           (parse-from-oanda)
                           :transactions
                           (parse-transactions))]
            (timbre/debug "[transactions-history] - get history sucessful")
            result)
          (timbre/error "[transaction-history] -" [account-id] "- error" (:status resp)
                        (json/parse-string (:body resp) ->kebab-case-keyword)))))))


(s/defn transactions-closed :- (s/maybe TransactionsResponse)
  [account :- OandaAccount]
  (when-let [transactions (transactions-history account)]
    (->> transactions
         (filter #(#{:trade-close :stop-loss-filled :take-profit-filled :trailing-stop-filled} (:type %)))
         (sort-by :time)
         (vec))))


(s/defn new-order! :- (s/maybe OrderCreateResponse)
  [{:keys [account-id url token headers]} :- OandaAccount query :- OrderCreateQuery]
  (when-let [resp @(http/post (path url "v1" "accounts" account-id "orders")
                              {:throw-exceptions false
                               :headers          headers
                               :oauth-token      token
                               :form-params      (parse-to-oanda query)})]
    (if (= 200 (:status resp))
      (let [result (-> resp
                       :body
                       (json/parse-string ->kebab-case-keyword)
                       (parse-from-oanda))]
        (timbre/debug "[new-order!] - post new order sucessful")
        result)
      (timbre/error "[new-order!] -" [account-id] "- error" (:status resp)
                    (json/parse-string (:body resp) ->kebab-case-keyword)))))

(s/defn market-order! :- (s/maybe OrderCreateResponse)
  ([account :- OandaAccount instrument :- s/Keyword side :- (s/enum :buy :sell)
    units :- Long stop-loss :- Double take-profit :- Double
    lower-bound :- Double upper-bound :- Double]
    (let [pip (instrument-pip account instrument)
          query {:instrument  instrument
                 :units       units
                 :side        side
                 :type        :market
                 :stop-loss   (precision stop-loss (/ pip 10))
                 :take-profit (precision take-profit (/ pip 10))
                 :lower-bound (precision lower-bound (/ pip 10))
                 :upper-bound (precision upper-bound (/ pip 10))}]
      (new-order! account query)))
  ([account :- OandaAccount instrument :- s/Keyword side :- (s/enum :buy :sell)
    units :- Long stop-loss :- Double take-profit :- Double]
    (let [query {:instrument  instrument
                 :units       units
                 :side        side
                 :type        :market
                 :stop-loss   (precision stop-loss (/ (instrument-pip account instrument) 10))
                 :take-profit (precision take-profit (/ (instrument-pip account instrument) 10))}]
      (new-order! account query)))
  ([account :- OandaAccount instrument :- s/Keyword side :- (s/enum :buy :sell) units :- Long]
    (let [query {:instrument instrument
                 :units      units
                 :side       side
                 :type       :market}]
      (new-order! account query))))


(s/defn market-if-touched-order! :- (s/maybe OrderCreateResponse)
  ([account :- OandaAccount instrument :- s/Keyword side :- (s/enum :buy :sell) price :- Double
    units :- Long stop-loss :- Double take-profit :- Double expiry :- DateTime]
    (let [query {:instrument  instrument
                 :units       units
                 :side        side
                 :price       price
                 :type        :market-if-touched
                 :stop-loss   (precision stop-loss (/ (instrument-pip account instrument) 10))
                 :take-profit (precision take-profit (/ (instrument-pip account instrument) 10))
                 :expiry      (long (/ (dtc/to-long expiry) 1000))}]
      (new-order! account query)))
  ([account :- OandaAccount instrument :- s/Keyword side :- (s/enum :buy :sell) price :- Double
    units :- Long expiry :- DateTime]
    (let [query {:instrument instrument
                 :units      units
                 :side       side
                 :price      price
                 :type       :market-if-touched
                 :expiry     (long (/ (dtc/to-long expiry) 1000))}]
      (new-order! account query))))


(s/defn market-buy! :- (s/maybe OrderCreateResponse)
  ([account :- OandaAccount
    instrument :- s/Keyword units :- Long
    stop-loss :- Double take-profit :- Double]
    (let [query {:instrument  instrument
                 :units       units
                 :side        :buy
                 :type        :market
                 :stop-loss   (precision stop-loss (/ (instrument-pip account instrument) 10))
                 :take-profit (precision take-profit (/ (instrument-pip account instrument) 10))}]
      (new-order! account query)))
  ([account :- OandaAccount instrument :- s/Keyword units :- Long]
    (let [query {:instrument instrument
                 :units      units
                 :side       :buy
                 :type       :market}]
      (new-order! account query))))


(s/defn market-sell! :- (s/maybe OrderCreateResponse)
  ([account :- OandaAccount instrument :- s/Keyword units :- Long
    stop-loss :- Double take-profit :- Double]
    (let [query {:instrument  instrument
                 :units       units
                 :side        :sell
                 :type        :market
                 :stop-loss   (precision stop-loss (/ (instrument-pip account instrument) 10))
                 :take-profit (precision take-profit (/ (instrument-pip account instrument) 10))}]
      (new-order! account query)))
  ([account :- OandaAccount instrument :- s/Keyword units :- long]
    (let [query {:instrument instrument
                 :units      units
                 :side       :sell
                 :type       :market}]
      (new-order! account query))))

;TODO modify-order!
;TODO close-order!

(s/defn modify-trade! :- (s/maybe TradeModifyResponse)
  ([{:keys [account-id url token headers]} :- OandaAccount
    {:keys [trade-id] :as query} :- TradeModifyQuery]
    (when-let [resp @(http/patch (path url "v1" "accounts" account-id "trades" trade-id)
                                 {:throw-exceptions false
                                  :headers          headers
                                  :oauth-token      token
                                  :form-params      (parse-to-oanda query)})]
      (if (= 200 (:status resp))
        (let [result (-> resp
                         :body
                         (json/parse-string ->kebab-case-keyword)
                         (parse-from-oanda))]
          (timbre/debug "[modify-trade!] - modify" trade-id "sucessful")
          result)
        (timbre/error "[modify-trade!] -" [account-id] "- error" (:status resp)
                      (json/parse-string (:body resp) ->kebab-case-keyword)))))
  ([account :- OandaAccount trade-id :- long stop-loss :- s/Num take-profit :- s/Num]
    (let [{old-sl :stop-loss
           old-tp :take-profit} (when-not (and stop-loss take-profit)
                                  (trade-info account trade-id))
          query {:trade-id    trade-id
                 :stop-loss   (or stop-loss old-sl 0)
                 :take-profit (or take-profit old-tp 0)}]
      (modify-trade! account query))))



(s/defn close-trade! :- (s/maybe TradeCloseResponse)
  [{:keys [account-id url token headers]} :- OandaAccount trade-id :- long]
  (when-let [resp @(http/delete (path url "v1" "accounts" account-id "trades" trade-id)
                                {:throw-exceptions false
                                 :headers          headers
                                 :oauth-token      token})]
    (if (= 200 (:status resp))
      (let [result (-> resp
                       :body
                       (json/parse-string ->kebab-case-keyword)
                       (parse-from-oanda))]
        (timbre/debug "[close-trade!] - close trade" trade-id "sucessful")
        result)
      (timbre/error "[close-trade!] -" [account-id] "- error" (:status resp)
                    (json/parse-string (:body resp) ->kebab-case-keyword)))))


;(defprotocol OandaAccountResources
;  (account-info [account])
;  (account-name [account])
;  (account-currency [account])
;  (margin-available [account])
;  (margin-used [account])
;  (margin-rate [account])
;  (balance [account])
;  (open-trades [account])
;  (unrealized-profit [account])
;  (realized-profit [account]))
;
;
;(defprotocol OandaOrdersResources
;  (orders-info [account])
;  (order-info [account order-id]))
;
;
;(defprotocol OandaTradesResources
;  (trades-info [account])
;  (trade-info [account trade-id]))
;
;
;(defprotocol OandaInstrumentsResources
;  (instruments-info [account] [account instruments])
;  (instrument-info [account instrument])
;  (instrument-display-name [account instrument])
;  (instrument-pip [account instrument])
;  (instrument-max-trade-units [account instrument])
;  (instrument-max-trailing-stop [account instrument])
;  (instrument-margin-rate [account instrument])
;  (instrument-halted? [account instrument])
;  (instruments-available [account])
;  (instruments-active [account]))
;
;
;(defprotocol OandaPricesResources
;  (prices-info [account] [account instruments])
;  (price-info [account instrument])
;  )