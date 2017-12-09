(ns machine.statistics.core
  (:require [schema.core :as s]
            [plumbing.core :refer [defnk fnk]]
            [plumbing.graph :as graph]
            [clj-time.core :as dt]
            [taoensso.encore :refer [round reset-in!]]
            [machine.settings.core :refer [settings]]
            [machine.utils :refer :all]
            [machine.money-management.core :as mm]
            [machine.oanda.core :as oanda])
  (:import [org.apache.commons.math3.stat StatUtils]
           [org.apache.commons.math3.stat.descriptive.moment StandardDeviation]
           [org.apache.commons.math3.distribution NormalDistribution]
           [org.apache.commons.math3.util FastMath]))


(defn z-score [pips]
  (let [f (graph/compile
            {:n       (fnk [xs] (count xs))
             :r       (fnk [xs] (count (partition-by pos? xs)))
             :w       (fnk [xs] (count (filter pos? xs)))
             :l       (fnk [xs] (count (filter neg? xs)))
             :p       (fnk [w l] (* 2 w l))
             :z-score (fnk [n r p]
                        (when (every? #(and (some? %) (pos? %))
                                      [n r p])
                          (- 1 (.density (NormalDistribution.)
                                         (/ (- (* n (- r 0.5)) p)
                                            (FastMath/pow ^double (/ (* p (- p n))
                                                                     (- n 1))
                                                          0.5))))))})]
    (:z-score (f {:xs pips}))))



(defn return-excess
  " Returns minus risk-free rate(s)."
  [r rf]
  {:pre [(or (number? rf) (= (count rf) (count r)))]}
  (if (number? rf)                                          ; return minus risk free rate(s)
    (mapv #(- % rf) r)
    (mapv #(- %1 %2) r rf)))


(defn sharpe-ratio
  " Returns the Sharpe ratio of ret, where ret is a vector of returns
    with an uniform time period, e.g. monthly returns.

    Options:
    :rf a single risk-free rate or a set of risk free rates,
        in same time period as returns.
    Reference:
    http://en.wikipedia.org/wiki/Sharpe_ratio
  "
  [ret & {:keys [rf] :or {rf 0}}]
  (let [r-rf (return-excess ret rf)]
    (/ (StatUtils/mean (double-array r-rf))
       (.evaluate (StandardDeviation.) (double-array r-rf)))))


(defn downside-risk
  " Downside risk, or the target semi-deviation. Calculated by finding the
    root mean squared underperformance, where the underperformance is the amount
    by which a return is below target."
  [r mar]
  (let [sub-r (filter #(< % mar) r)]
    (when (pos? (count sub-r))
      (FastMath/sqrt ^double (/ (reduce + (map #(FastMath/pow ^double (- % mar) 2.0) sub-r))
                                (count sub-r))))))

(defn sortino-ratio
  " Returns the Sortino ratio of ret, where ret is a vector of returns
    with an uniform time period, e.g. monthly returns.

    Options:
    :mar minimum acceptable return, or target rate of return. Must be
         in same time period as ret.
    Reference:
    http://en.wikipedia.org/wiki/Sortino_ratio
  "
  [ret & {:keys [mar] :or {mar 0}}]
  (let [ex-r (return-excess ret mar)
        ds-risk (downside-risk ret mar)]
    (when (and ds-risk (pos? ds-risk))
      (/ (StatUtils/mean (double-array ex-r)) ds-risk))))

;(defn max-drawndown-percent
;  [xs]
;  (loop [peak 0.0 max-dd 0.0 [last current & r] xs]
;    (if-not (nil? current)
;      (let [profit (- current last)]
;        (if (neg? profit)
;          (let [dd (- peak current)
;                dd% (/ dd peak)]
;            (recur peak (FastMath/max max-dd dd%) r))
;          (recur (double current) max-dd r)))
;      max-dd)))


(s/defn add-money-management :- [{:instrument            s/Keyword
                                  :open-price            s/Num
                                  :entry-stop-loss-price s/Num
                                  :units                 (s/maybe s/Num)
                                  :pips-profit           s/Num
                                  :profit                s/Num
                                  :account-balance       s/Num
                                  s/Keyword              s/Any}]
  [{:keys [trade-size-fn start-balance max-risk max-dd
           window]} :- {:trade-size-fn           s/Any
                        :start-balance           s/Num
                        :max-risk                s/Num
                        :max-dd                  s/Num
                        (s/optional-key :window) s/Num}
   history-trades :- [{s/Keyword s/Any}]]
  (let [sorted-trades (sort-by (juxt :close-time :open-time) history-trades)]
    (loop [balance [start-balance] [trade & rest-trades] sorted-trades result-trades []]
      (if-not (nil? trade)
        (let [{:keys [instrument open-price entry-stop-loss-price pips-profit]} trade
              instrument-tick-value (oanda/instrument-tick-value (first oanda/oanda-accounts)
                                                                 instrument)]
          (if-let [trade-size (trade-size-fn {:account         (first oanda/oanda-accounts)
                                              :instrument      instrument
                                              :open-price      open-price
                                              :stop-loss-price entry-stop-loss-price
                                              :max-dd          max-dd
                                              :max-risk        max-risk
                                              :balance         (last balance)
                                              :window          window}
                                             result-trades)]
            (let [profit (* instrument-tick-value trade-size pips-profit)
                  account-balance (+ (last balance) profit)
                  result-trade (assoc trade
                                 :units trade-size
                                 :profit profit
                                 :account-balance (last balance))]
              (recur (conj balance account-balance) rest-trades (conj result-trades result-trade)))
            (recur (conj balance (last balance)) rest-trades result-trades)))
        result-trades))))


(s/defschema TradingResultStatistic
  {:start-balance   (s/maybe s/Num)
   ;:end-balance     (s/maybe s/Num)
   :max-drawndown%  (s/maybe s/Num)
   :trades-count    (s/maybe s/Num)
   :duration-avg    (s/maybe s/Num)
   :profit          (s/maybe s/Num)
   :profit%         (s/maybe s/Num)
   :pips-profit     (s/maybe s/Num)
   :trades-win      (s/maybe s/Num)
   :pips-win        (s/maybe s/Num)
   :trades-loss     (s/maybe s/Num)
   :pips-loss       (s/maybe s/Num)
   :pips-win-avg    (s/maybe s/Num)
   :pips-loss-avg   (s/maybe s/Num)
   :cash-win        (s/maybe s/Num)
   :cash-loss       (s/maybe s/Num)
   :cash-win-avg    (s/maybe s/Num)
   :cash-loss-avg   (s/maybe s/Num)
   :rr              (s/maybe s/Num)
   :profit-factor   (s/maybe s/Num)
   :profitability   (s/maybe s/Num)
   :expectancy-pips (s/maybe s/Num)
   :expectancy-cash (s/maybe s/Num)
   :z-score         (s/maybe s/Num)
   :sharpe-ratio    (s/maybe s/Num)
   :sortino-ratio   (s/maybe s/Num)})


(def ^:private trading-result-statistic-graph
  {:start-balance   (fnk [xs] (->> xs (sort-by :open-time) (first) :account-balance))
   ;:end-balance     (fnk [xs] (let [last-trade (->> xs (sort-by :close-time) (last))]
   ;                             (+ (:account-balance last-trade) (:profit last-trade))))
   :pips            (fnk [xs] (remove nil? (map :pips-profit (sort-by :close-time xs))))
   :daily-pips      (fnk [xs] (->> xs
                                   (group-by (fn [{:keys [open-time close-time]}]
                                               (dt/in-days (dt/interval (dt/date-time (dt/year (or close-time open-time)))
                                                                        close-time))))
                                   (map (fn [[_ coll]]
                                          (reduce + (map :pips-profit coll))))))
   :cash            (fnk [xs] (remove nil? (map :profit (sort-by :close-time xs))))
   :daily-cash      (fnk [xs] (->> xs
                                   (group-by (fn [{:keys [open-time close-time]}]
                                               (dt/in-days (dt/interval (dt/date-time (dt/year (or close-time open-time)))
                                                                        close-time))))
                                   (map (fn [[_ coll]]
                                          (reduce + (map :profit coll))))))
   :max-drawndown%  (fnk [xs] (let [dd (mm/max-peak-valey-drawndown-percent xs)] dd))
   :trades-count    (fnk [xs] (count xs))
   :duration-avg    (fnk [xs] (StatUtils/mean (double-array (->> xs (map :duration) (remove nil?)))))
   :profit          (fnk [cash] (reduce + cash))
   :profit%         (fnk [start-balance profit] (when (and profit start-balance)
                                                  (/ profit start-balance)))
   :pips-profit     (fnk [pips] (reduce + pips))
   :trades-win      (fnk [cash] (count (filter pos? cash)))
   :pips-win        (fnk [pips] (reduce + (filter pos? pips)))
   :trades-loss     (fnk [cash] (count (filter neg? cash)))
   :pips-loss       (fnk [pips] (- (reduce + (filter neg? pips))))
   :pips-win-avg    (fnk [trades-win pips-win]
                      (if (and (some? trades-win) (pos? trades-win)
                               (some? pips-win) (pos? pips-win))
                        (/ pips-win trades-win)))
   :pips-loss-avg   (fnk [trades-loss pips-loss]
                      (when (and (some? trades-loss) (pos? trades-loss)
                                 (some? pips-loss) (pos? pips-loss))
                        (/ pips-loss trades-loss)))
   :cash-win        (fnk [cash] (reduce + (filter pos? cash)))
   :cash-loss       (fnk [cash] (- (reduce + (filter neg? cash))))
   :cash-win-avg    (fnk [trades-win cash-win]
                      (if (and (some? trades-win) (pos? trades-win)
                               (some? cash-win) (pos? cash-win))
                        (/ cash-win trades-win)))
   :cash-loss-avg   (fnk [trades-loss cash-loss]
                      (when (and (some? trades-loss) (pos? trades-loss)
                                 (some? cash-loss) (pos? cash-loss))
                        (/ cash-loss trades-loss)))
   :rr              (fnk [pips-win-avg pips-loss-avg]
                      (when (and (some? pips-win-avg) (pos? pips-win-avg)
                                 (some? pips-loss-avg) (pos? pips-loss-avg))
                        (/ pips-loss-avg pips-win-avg)))
   :profit-factor   (fnk [cash-win cash-loss]
                      (when (and (some? cash-win) (pos? cash-win)
                                 (some? cash-loss) (pos? cash-loss))
                        (/ cash-win cash-loss)))
   :profitability   (fnk [trades-win trades-loss]
                      (when (and (some? trades-win)
                                 (some? trades-loss) (pos? trades-loss))
                        (if-not (zero? trades-win) (double (/ trades-win (+ trades-win trades-loss))) 0.0)))
   :expectancy-pips (fnk [pips-win-avg pips-loss-avg profitability]
                      (when (every? #(and (some? %) (pos? %)) [pips-win-avg pips-loss-avg profitability])
                        (- (* profitability pips-win-avg)
                           (* (- 1 profitability) pips-loss-avg))))
   :expectancy-cash (fnk [cash-win-avg cash-loss-avg profitability]
                      (when (every? #(and (some? %) (pos? %)) [cash-win-avg cash-loss-avg profitability])
                        (- (* profitability cash-win-avg)
                           (* (- 1 profitability) cash-loss-avg))))
   :z-score         (fnk [pips] (z-score pips))
   :sharpe-ratio    (fnk [daily-cash] (sharpe-ratio daily-cash))
   :sortino-ratio   (fnk [daily-cash] (sortino-ratio daily-cash))
   :car-maxdd%      (fnk [profit% max-drawndown%] (when (pos? max-drawndown%)
                                                    (/ profit% max-drawndown%)))
   :payoff-ratio    (fnk [cash-win-avg cash-loss-avg] (when (pos? cash-loss-avg)
                                                        (/ cash-win-avg cash-loss-avg)))})


(def ^:private trading-result-statistic
  (graph/compile trading-result-statistic-graph))


(s/defn descriptive :- TradingResultStatistic
  [trades :- [{:instrument            s/Keyword
               :open-price            s/Num
               :entry-stop-loss-price s/Num
               :pips-profit           s/Num
               s/Keyword              s/Any}]]
  (-> (trading-result-statistic {:xs trades})
      (dissoc :pips :daily-pips :cash :daily-cash)
      (->> (into (sorted-map)))))


(s/defn full :- (assoc TradingResultStatistic
                  :mm-start-balance s/Num
                  :mm-max-risk s/Num
                  :mm-max-dd s/Num)
  [{:keys [start-balance max-risk max-dd] :as opts} :- {:trade-size-fn           s/Any
                                                        :start-balance           s/Num
                                                        :max-risk                s/Num
                                                        :max-dd                  s/Num
                                                        (s/optional-key :window) s/Num}
   trades :- [{:instrument            s/Keyword
               :open-price            s/Num
               :entry-stop-loss-price s/Num
               :pips-profit           s/Num
               s/Keyword              s/Any}]]
  (when-let [result (descriptive (add-money-management opts trades))]
    (-> result
        (assoc :mm-start-balance start-balance :mm-max-risk max-risk :mm-max-dd max-dd)
        (->> (into (sorted-map))))))