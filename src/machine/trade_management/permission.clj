(ns machine.trade-management.permission
  (:require [schema.core :as s]
            [clj-time.core :as dt]
            [taoensso.timbre :as timbre]
            [machine.settings.core :refer [settings]]
            [machine.oanda.core :as oanda]
            [machine.time-management.market-hours :as market-hours])
  (:import [org.joda.time DateTime Seconds]
           [machine.oanda.core OandaAccount]))


(s/defn positive-trade-size? :- s/Bool
  [trade-size :- s/Num]
  (if (pos? trade-size)
    (do (timbre/info "[permission-watchman] - trade size is positive. Trading allowed")
        true)
    (do (timbre/warn "[permission-watchman] - trade size is" trade-size ". Trading not allowed")
        true)))


(s/defn time-slippage-alloved? :- s/Bool
  [signal-time :- DateTime max-time-slippage :- Seconds]
  (if (dt/before? (dt/now) (dt/plus signal-time max-time-slippage))
    (do
      (timbre/info "[permission-watchman] - little time slippage. Trading allowed")
      true)
    (do
      (timbre/warn "[permission-watchman] - to big time slippage. Trading not allowed")
      false)))


(s/defn price-slippage-alloved? :- s/Bool
  [account :- OandaAccount {:keys [instrument side open-price stop-loss-price]}]
  (case side
    :buy (let [bid-price (oanda/price-ask account instrument)]
           (if (and (> bid-price stop-loss-price) (<= bid-price open-price))
             (do (timbre/info "[permission-watchman] - little price slippage. Trading allowed")
                 true)
             (do (timbre/warn "[permission-watchman] - to big price slippage. Trading not allowed")
                 false)))
    :sell (let [ask-price (oanda/price-ask account instrument)]
            (if (and (< ask-price stop-loss-price) (>= ask-price open-price))
              (do (timbre/info "[permission-watchman] - little price slippage. Trading allowed")
                  true)
              (do (timbre/warn "[permission-watchman] - to big price slippage. Trading not allowed")
                  false)))))


(s/defn instrument-active? :- s/Bool
  [account :- OandaAccount instrument :- s/Keyword]
  (if (oanda/instrument-halted? account instrument)
    (do (timbre/info "[permission-watchman] - instrument is halted. Trading not allowed")
        false)
    (do
      (timbre/warn "[permission-watchman] - instrument is not halted. Trading allowed")
      true)))

(s/defn margin-allowed? :- s/Bool
  [account :- OandaAccount min-margin :- s/Num]
  (if-let [margin-percent (* 100 (/ (oanda/margin-available account)
                                    (max (oanda/margin-used account) 1.0)))]
    (if (> margin-percent min-margin)
      (do (timbre/info "[permission-watchman] - margin percent"
                       margin-percent ". It is greater than the minimum allowable"
                       min-margin ". Trading allowed")
          true)
      (do
        (timbre/warn "[permission-watchman] - margin percent"
                     margin-percent ". It is lestt than the minimum allowable"
                     min-margin ". Trading not allowed")
        false))
    false))


(s/defn spread-allowed? :- s/Bool
  ([spread :- s/Num max-spread :- s/Num]
    (<= spread max-spread))
  ([account :- OandaAccount instrument :- s/Keyword max-spread :- s/Num]
    (let [actual-spread (oanda/instrument-spread account instrument)]
      (if (<= actual-spread max-spread)
        (do
          (timbre/info "[permission-watchman] -" instrument "spread is"
                       (* actual-spread (oanda/instrument-pip account instrument))
                       ". It is less than the maximum allowable spread"
                       (* max-spread (oanda/instrument-pip account instrument))
                       ". Trading allowed")
          true)
        (do
          (timbre/warn "[permission-watchman] -" instrument "spread is"
                       (* actual-spread (oanda/instrument-pip account instrument))
                       ". It is greater than the maximum allowable spread"
                       (* max-spread (oanda/instrument-pip account instrument))
                       ". Trading not allowed")
          false)))))


(s/defn trades-count-allowed? :- s/Bool
  [account :- OandaAccount max-open-trades :- s/Num]
  (if-let [trades-count (oanda/open-trades account)]
    (cond
      (zero? max-open-trades)
      (do
        (timbre/warn "[permission-watchman] - max open trades is undefined. Trading allowed")
        true)

      (< trades-count max-open-trades)
      (do
        (timbre/info "[permission-watchman] - trades count is"
                     trades-count ". It is less than the maximum allowable trades" max-open-trades
                     ". Trading allowed")
        true)

      (>= trades-count max-open-trades)
      (do
        (timbre/warn "[permission-watchman] - trades count is"
                     trades-count ". It is greater than the maximum allowable trades" max-open-trades
                     ". Trading allowed")
        false))
    false))


(s/defn instrument-allowed? :- (s/maybe s/Bool)
  [account :- OandaAccount instrument :- s/Keyword]
  (when-let [trades-info (oanda/trades-info account)]
    (let [instrument-used? (->> trades-info
                                (map :instrument)
                                (mapcat (fn [instrument] (clojure.string/split (name instrument) #"-")))
                                (distinct)
                                (some (set (clojure.string/split (name instrument) #"-"))))]
      (if-not instrument-used?
        (do
          (timbre/info "instrument " instrument "is not used")
          true)
        (do
          (timbre/warn "instrument " instrument "is used!")
          false)))))


(s/defn sharpe-ratio-allowed? :- s/Bool
  [sharpe-ratio min-sharpe-ratio]
  (if (or (nil? sharpe-ratio) (> sharpe-ratio min-sharpe-ratio))
    (do
      (timbre/info "[permission-watchman] - sharpe-ratio is" sharpe-ratio
                   ". It is greater than the minimum allowable ratio" min-sharpe-ratio
                   ". Trading allowed")
      true)
    (do
      (timbre/info "[permission-watchman] - sharpe-ratio is" sharpe-ratio
                   ". It is less than the minimum allowable ratio" min-sharpe-ratio
                   ". Trading not allowed")
      false)))


(s/defn sortino-ratio-allowed? :- s/Bool
  [sortino-ratio min-sortino-ratio]
  (if (or (nil? sortino-ratio) (> sortino-ratio min-sortino-ratio))
    (do
      (timbre/info "[permission-watchman] - sortino-ratio is" sortino-ratio
                   ". It is greater than the minimum allowable ratio" min-sortino-ratio)
      true)
    (do
      (timbre/info "[permission-watchman] - sortino-ratio is" sortino-ratio
                   ". It is less than the minimum allowable ratio" min-sortino-ratio)
      false)))


(s/defn market-allowed?
  [t :- DateTime markets :- [s/Keyword]]
  (if-not (empty? markets)
    (if-let [intersection (vec (clojure.set/intersection (set (market-hours/active-markets t))
                                                         (set markets)))]
      (do
        (timbre/info "[permission-watchman] - market intersection" intersection
                     ". Trading allowed")
        true)
      (do
        (timbre/info "[permission-watchman] - no market intersection. Trading not allowed")
        false))
    (do
      (timbre/info "[permission-watchman] - all market allowed. Trading allowed")
      true)))