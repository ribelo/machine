(ns machine.repl
  (:require [clojure.core.async :as async
             :refer [go go-loop chan <! <!! >! >!! timeout
                     put! take! close! alts!]]
            [clojure.pprint :as pprint]
            [clj-time.core :as dt]

            [clojure.math.combinatorics :refer [cartesian-product] :as comb]
            [cheshire.core :as json]
            [mount.core :as mount]
            [schema.core :as s]
            [taoensso.encore :refer [round dissoc-in distinct-by when-lets]]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as appenders]
            [machine.settings.core :refer [settings]]
            [machine.db.core :as db]
            [machine.router.core :as router]
            [machine.quotes.core :as q]
            [machine.quotes.series :refer :all]
            [machine.oanda.core :as oanda]
            [machine.data-management.core :as dm]
            [machine.time-management.core :as tim]
            [machine.analyse-management.core :as analyse]
            [machine.analyse-management.quantum.correlation :refer [pearsons-correlation]]
    ;[machine.analyse-management.wrb-analysis.vtr :as vtr]

            [machine.analyse-management.candle.core :as candle]
            [machine.analyse-management.wrb-analysis.wrb :as wrb]
            [machine.analyse-management.wrb-analysis.zone :as zone]

            [machine.strategy.fvb :refer [FVB]]
            [machine.strategy.vtr :refer [VTR]]
            [machine.strategy.vsa :refer [VSA]]
            [machine.strategy.vtr-h2 :refer [VTR-H2]]
            [machine.strategy.core :as strategy]
            [machine.statistics.core :as stats]
            [criterium.core :refer [quick-bench]]
            [machine.strategy.core :as strategy]
            [machine.money-management.core :as mm]
    ;[incanter.charts :as charts]
            [incanter.core :refer [view save]]
            [mount.tools.graph]
            [machine.oanda.workers]
            [machine.data-management.workers]
            [machine.analyse-management.workers]
            [machine.time-management.workers]
            [machine.trade-management.workers]
            [machine.analyse-management.wrb-analysis.fvb :as fvb]
            [machine.analyse-management.wrb-analysis.vtr :as vtr]
            [machine.analyse-management.wrb-analysis.vsa :as vsa]
            [clj-time.coerce :as dtc]
            [machine.settings.core :as settings]
            [machine.analyse-management.seiden.core :as sd])
  (:import [org.apache.commons.math3.stat.correlation PearsonsCorrelation]
           [org.apache.commons.lang3 ArrayUtils]
           [machine.quotes.core Quotes]
           [org.apache.commons.math3.util FastMath]))

(s/set-fn-validation! false)

(timbre/merge-config! {:appenders {
                                   ;:spit    (assoc (appenders/spit-appender {:fname "log.txt"})
                                   ;           :min-level :warn),
                                   :println {:min-level :warn
                                             :async?    true}}})

(mapv mount/start [#'db/DB #'settings #'dm/instruments #'dm/periods #'oanda/oanda-accounts
                   #'router/channel #'router/publication #'VTR #'VTR-H2 #'FVB #'VSA])

(mount/stop #'VTR #'VTR-H2 #'FVB)
(mount/stop #'db/DB)
(mount/stop #'settings #'dm/instruments #'dm/periods #'oanda/oanda-accounts #'VTR #'FVB)

(mount.tools.graph/states-with-deps)

(mount/start)
(mount/stop)
(oanda/instruments-available (first oanda/oanda-accounts))


(defn download-quotes
  ([instruments periods candle-count]
   (let [partitioned-instruments (partition-all (round (/ (count instruments) 4))
                                                instruments)]
     (->> (map vector (range) partitioned-instruments)
          (mapv (fn [[idx worker-instruments]]
                  (go
                    (println idx worker-instruments)
                    (doseq [instrument worker-instruments
                            period periods]
                      (if (db/get-quotes instrument period)
                        (when-let [data (dm/update-quotes (first oanda/oanda-accounts) instrument period)]
                          (println "worker" idx "update" instrument period "sucessful!")
                          (db/save-quotes! instrument period data))
                        (when-let [data (dm/download-quotes (first oanda/oanda-accounts) instrument period candle-count)]
                          (println "worker" idx "download" instrument period "sucessful!")
                          (db/save-quotes! instrument period data))))))))))
  ([]
   (download-quotes dm/instruments dm/periods 1024)))
(download-quotes dm/instruments dm/periods (* 10 4096))

(partition-all (round (/ (count dm/instruments) 4))
               dm/instruments)

(defn analyse-quotes
  ([instruments periods]
   (->> (cartesian-product instruments periods)
        (pmap (fn [[instrument period]]
                (when-let [data (db/get-quotes instrument period)]
                  (->> data
                       (analyse/analyse-quotes)
                       (db/save-quotes! instrument period))
                  (println "refresh " instrument " " period))))
        (doall))
   nil)
  ([data]
   (->> data
        (analyse/analyse-quotes)))
  ([]
   (analyse-quotes dm/instruments
                   dm/periods)))
(analyse-quotes)


(defn correlation-analyse
  ([instruments periods length]
   (let [all-data (mapv (fn [[instrument period]]
                          (db/get-quotes instrument period))
                        (comb/cartesian-product instruments periods))]
     (->> all-data
          (pmap (fn [{main-instrument :instrument :keys [period] :as main-data}]
                  (let [analysed (->> all-data
                                      (remove #(or (= main-instrument (:instrument %))
                                                   (not= period (:period %))))
                                      (reduce (fn [d1 d2]
                                                (pearsons-correlation d1 d2 length))
                                              main-data))]
                    (println "correlation " [main-instrument] " " period)
                    (db/save-quotes! main-instrument period analysed))))
          (doall)))
   nil)
  ([main-data length]
   (let [main-instrument (:instrument main-data)
         period (:period main-data)
         analysed (->> dm/instruments
                       (remove #(= main-instrument %))
                       (map #(db/get-quotes % period))
                       (reduce (fn [d1 d2]
                                 (pearsons-correlation d1 d2 length))
                               main-data))]
     analysed))
  ([length]
   (correlation-analyse dm/instruments dm/periods length)))
(correlation-analyse 1024)


(defn vtr-analyse
  ([instruments periods zone-types]
   (doseq [instrument instruments
           period periods]
     (when-let [data (db/get-quotes instrument period)]
       (db/save-quotes! instrument period (dissoc data :vtr# :vtr-zone-nr#))))
   (doseq [main-instrument instruments
           period periods]
     (when-lets [main-data (db/get-quotes main-instrument period)
                 analysed (->> instruments
                               (remove #(= main-instrument %))
                               (map #(db/get-quotes % period))
                               (reduce (fn [d1 d2]
                                         (vtr/volatility-trading-report d1 d2 zone-types))
                                       main-data))]
                (println "vtr " [main-instrument] " " period)
                (db/save-quotes! main-instrument period analysed))))
  ([main-data zone-types]
   (let [main-instrument (:instrument main-data)
         period (:period main-data)
         analysed (->> dm/instruments
                       (remove #(= main-instrument %))
                       (map #(db/get-quotes % period))
                       (reduce (fn [d1 d2]
                                 (vtr/volatility-trading-report d1 d2 zone-types))
                               main-data))]
     analysed))
  ([zone-types]
   (vtr-analyse dm/instruments
                dm/periods
                zone-types)))
(vtr-analyse [:zone-sp1])

(defn vtr-h2-analyse
  ([instruments periods zone-types]
   (doseq [instrument instruments
           period periods]
     (when-let [data (db/get-quotes instrument period)]
       (db/save-quotes! instrument period (dissoc data :vtr-h2# :vtr-h2-zone-nr#))))
   (doseq [main-instrument instruments
           period periods]
     (when-lets [main-data (db/get-quotes main-instrument period)
                 analysed (->> instruments
                               (remove #(= main-instrument %))
                               (map #(db/get-quotes % period))
                               (reduce (fn [d1 d2]
                                         (vtr/volatility-trading-report-h2 d1 d2 zone-types))
                                       main-data))]
                (println "vtr-h2 " [main-instrument] " " period)
                (db/save-quotes! main-instrument period analysed))))
  ([main-data zone-types]
   (let [main-instrument (:instrument main-data)
         period (:period main-data)
         analysed (->> dm/instruments
                       (remove #(= main-instrument %))
                       (map #(db/get-quotes % period))
                       (reduce (fn [d1 d2]
                                 (vtr/volatility-trading-report-h2 d1 d2 zone-types))
                               main-data))]
     analysed))
  ([zone-types]
   (vtr-h2-analyse dm/instruments
                   dm/periods
                   zone-types)))

(vtr-h2-analyse [:zone-sp1])

(defn fvb-analyse
  ([instruments periods zone-types]
   (doseq [instrument instruments
           period periods]
     (let [data (db/get-quotes instrument period)]
       (db/save-quotes! instrument period (dissoc data :fvb# :fvb-zone-nr#))))
   (doseq [instrument instruments
           period periods]
     (let [data (db/get-quotes instrument period)]
       (let [analysed (fvb/fade-volatility-breakout data zone-types)]
         (db/save-quotes! instrument period analysed))))
   nil)
  ([zone-types]
   (fvb-analyse dm/instruments dm/periods zone-types)))

(fvb-analyse [:zone-sp1])

(let [data (db/get-quotes :eur-usd :m15)]
  (:fvb# data))

(defn vsa-analyse
  ([instruments periods zone-types]
   (doseq [instrument instruments
           period periods]
     (let [data (db/get-quotes instrument period)]
       (db/save-quotes! instrument period (dissoc data :vsa-sd# :vsa-sd-zone-nr#))))
   (doseq [instrument instruments
           period periods]
     (let [data (db/get-quotes instrument period)]
       (let [analysed (vsa/supply-demand data zone-types)]
         (db/save-quotes! instrument period analysed))))
   nil)
  ([zone-types]
   (vsa-analyse dm/instruments dm/periods zone-types)))
(vsa-analyse [:zone-sp1])



(let [main-data (db/get-quotes :eur-usd :m5)]
  (count (distinct (map #(machine.time-management.core/candle-time :d %) (:time# main-data)))))

(def result
  (->> (comb/cartesian-product
         dm/instruments
         [:m5 :m10 :m15])
       (pmap (fn [[instrument period]]
               (let [data (db/get-quotes instrument period)
                     trades (strategy/history-trades
                              (first VTR) data
                              {:rr               3
                               :min-rr           nil
                               :profit-target    nil
                               :trailing-stop?   nil
                               :trailing-profit? nil
                               :break-even?      nil
                               :dcm?             false
                               :zones            [:zone-sp1]
                               :markets          [:frankfurt :london :new-york
                                                  :tokyo :sydney :wellington]})]
                 (-> (stats/full {:trade-size-fn mm/redd-trade-size
                                  :start-balance 10000
                                  :max-risk      0.05
                                  :max-dd        0.25}
                                 trades)
                     (assoc :instrument instrument)
                     (assoc :period period)))))
       (doall)))
(->> result
     (filter #(not (Double/isNaN (:sharpe-ratio %))))
     (sort-by :sortino-ratio))


(->> result
     (map :profit)
     (reduce +))
17977.453878196833

(->> result
     (group-by :period)
     (map (fn [[period coll]] [period (reduce + (map :profit coll))]))
     (sort-by second)
     (reverse))




(->> result
     ;(filter #(= :m15 (:period %)))
     (group-by :instrument)
     (map (fn [[instrument coll]] [instrument (reduce + (map :profit coll))]))
     (sort-by #(nth % 0))
     ;(map second)
     ;(filter pos?)
     ;(reduce +)
     ;(count)
     ;(reverse)
     )
;(filter #(pos? (second %)))
;(map first)


(->> result
     (group-by (juxt :instrument :period))
     (map (fn [[[instrument period] coll]] [instrument period (reduce + (map :profit coll))]))
     ;(filter #(= :m15 (:period %)))
     (sort-by #(nth % 0)))
;(sort-by second)
;(reverse)



(->> result
     (group-by (juxt :instrument :period))
     (map (fn [[period coll]] [period (reduce + (map :profit coll))]))
     (sort-by last)
     (reverse))

(->> result
     (sort-by :close-pips)
     (reverse)
     (filter #(and (number? (:sharpe-ratio %)) (> (:sharpe-ratio %) 0.1)))
     ;(map :period)
     ;(frequencies)
     (map (fn [m] (select-keys m [:instrument :period :trades-count :profitability :rr :close-pips
                                  :sharpe-ratio :sortino-ratio :win-avg])))
     (sort-by :profitability)
     (reverse))
;(map :close-pips)
;(map :trades-count)
;(reduce +)

;(sort-by (juxt :trades-count :instrument))
;(pprint/print-table)
;(sort-by :sortino-ratio)
;(reverse)
;(sort-by last)




(->> result
     (group-by :instrument)
     (map (fn [[instrument coll]] {:instrument instrument
                                   :close-pips (reduce + (map :close-pips coll))}))
     (sort-by :close-pips)
     (reverse)
     (pprint/print-table))

(defn all-trades
  ([strg opts]
   (all-trades dm/instruments
               dm/periods
               strg opts))
  ([instruments periods strg opts]
   (->> (comb/cartesian-product instruments periods)
        (pmap (fn [[instrument period]]
                (let [data (db/get-quotes instrument period)]
                  (strategy/history-trades strg data opts))))
        (doall)
        (flatten)
        (sort-by (juxt :open-time :close-time))
        (reverse)
        (into []))))

(-> (all-trades
      dm/instruments dm/periods
      ;dm/instruments [:m5 :m10 :m15]
      VTR
      {:rr               3
       :min-rr           nil
       :profit-target    nil
       :trailing-stop?   nil
       :trailing-profit? nil
       :break-even?      nil
       :dcm?             false
       :markets          [:frankfurt :london :new-york
                          :tokyo :sydney :wellington]})
    (->>
      (sort-by :open-time)
      ;(map #(select-keys % [:instrument :period :side :open-time :close-time :pips-profit]))
      ;(filter #(= :eur-usd (:instrument %)))
      (take 5)))
;(map :pips-profit)
;(partition-by (fn [profit] (pos? profit)))
;(map (fn [coll] (if (pos? (first coll))
;                  (count coll)
;                  (- (count coll)))))

(let [data (db/get-quotes :gbp-chf :m5)]
  (vec (:correlation-aud-nzd# data)))

(let [instrument :eur-aud
      period :m5
      data (db/get-quotes instrument period)
      ;data (q/subvector main-data 80 1024)
      ;_ (println (aget (:time# data) 0))
      ;data (-> data (analyse-quotes) (correlation-analyse 1024) (vtr-analyse [:zone-sc2]))
      vtr-nr (inc (ArrayUtils/indexOf (:time# data) (dt/date-time 2016 5 2 15)))
      vtr-time (aget (:time# data) vtr-nr)
      vtr-dir (aget (:vtr# data) vtr-nr)
      vtr-sister-name (aget (:vtr-sister-name# data) vtr-nr)
      sister-data (db/get-quotes vtr-sister-name period)
      vtr-correlation (aget ((keyword (str "correlation-" (name vtr-sister-name) "#")) data) vtr-nr)
      ;zone-time (dt/date-time 2016 4 19 16 50)
      ;zone-nr (ArrayUtils/indexOf (:time# data) zone-time)
      zone-nr (aget (:vtr-zone-nr# data) vtr-nr)
      zone-time (aget (:time# data) zone-nr)
      zone-dir (aget (:zone-sp1# data) zone-nr)
      zone-confirmation-time (aget (:time# data) (aget (:zone-sp1-confirmation-candle# data) zone-nr))]
  {:vtr-time               vtr-time
   :vtr-nr                 vtr-nr
   :vtr-dir                vtr-dir
   :vtr-sister-name        vtr-sister-name
   :vtr-correlation        vtr-correlation
   :zone-nr                zone-nr
   :zone-time              zone-time
   :zone-dir               zone-dir
   :zone-confirmation-time zone-confirmation-time
   :zone-v2                (aget (:time# data) (+ zone-nr (aget (:zone-sc2-contraction-count# data) zone-nr)))}
  )
(mm/max-peak-valey-drawndown-percent (oanda/transactions-closed (first oanda/oanda-accounts)))

(let [data (db/get-quotes :eur-usd :m5)]
  [(:vtr-h2# data)])

(mount/stop #'settings #'oanda/oanda-accounts #'VTR #'VTR-H2 #'FVB #'VSA)
(map mount/start [#'settings #'oanda/oanda-accounts #'VTR #'VTR-H2 #'FVB #'VSA])

(let [trades (stats/add-money-management
               {:trade-size-fn mm/redd-trade-size
                :start-balance 10000
                :max-risk      0.05
                :max-dd        0.3
                :window        5}
               (->> (all-trades dm/instruments
                                [:m15]
                                (first VTR)
                                {:rr                 3
                                 :min-rr             nil
                                 :profit-target      nil
                                 :trailing-stop?     nil
                                 :trailing-profit?   nil
                                 :break-even?        nil
                                 :dcm?               false
                                 :max-spread-percent 0.15
                                 :markets            [:frankfurt :london :new-york
                                                      :tokyo :sydney :wellington]})
                    (distinct-by (juxt :period :open-time))
                    (sort-by :open-time)))
      balance (mm/balance-history (sort-by :close-time trades))]
  (view (charts/line-chart (range (count balance)) balance))
  [(sort-by :open-time trades)
   (sort-by first (frequencies (map (fn [{:keys [open-time]}]
                                      (dt/floor open-time dt/day))
                                    trades)))])

(oanda/instruments-available (first oanda/oanda-accounts))

(map (juxt :time# :candle-direction#) (take 10 (q/row-maps (db/get-quotes :eur-usd :m10))))
(dt/floor (dt/now) dt/week-number-of-year)

(download-quotes)
(analyse-quotes)
(correlation-analyse 1024)
(do (analyse-quotes)
    (correlation-analyse 1024)
    (vtr-analyse [:zone-sp2]))
(vtr-analyse [:zone-sp2])
(vtr-h2-analyse [:zone-sp1 :zone-sp2])
(fvb-analyse [:zone-sp1])
(vsa-analyse [:zone-sp1 :zone-sp2 :zone-sp3 :zone-sc1 :zone-sc2])


(->> (all-trades dm/instruments
                 [:m5 :m10 :m15]
                 (first VTR)
                 {:rr                 3
                  :min-rr             nil
                  :profit-target      nil
                  :trailing-stop?     nil
                  :trailing-profit?   nil
                  :break-even?        nil
                  :dcm?               false
                  :max-spread-percent 0.3
                  :markets            [:frankfurt :london :new-york
                                       :tokyo :sydney :wellington]})
     (distinct-by (juxt :period :open-time))
     (map #(select-keys % [:instrument :open-time :open-price :close-price :side :period :close-type :pips-profit]))
     (sort-by :open-time))


(stats/descriptive
  (stats/add-money-management
    {:trade-size-fn mm/redd-trade-size
     :start-balance 10000
     :max-risk      0.05
     :max-dd        0.3
     :window        10}
    (->> (all-trades dm/instruments
                     [:m5]
                     (first VTR)
                     {:rr                 3
                      :min-rr             nil
                      :profit-target      nil
                      :trailing-stop?     nil
                      :trailing-profit?   nil
                      :break-even?        nil
                      :dcm?               false
                      :max-spread-percent 0.5
                      :markets            [:frankfurt :london :new-york
                                           :tokyo :sydney :wellington]})
         (distinct-by (juxt :period :open-time))
         (sort-by :open-time))))

{:car-maxdd%      9.99314317354613,
 :cash-loss       7629.993429029763,
 :cash-loss-avg   401.57860152788226,
 :cash-win        15544.643833530597,
 :cash-win-avg    863.5913240850332,
 :duration-avg    16.756756756756758,
 :expectancy-cash 213.90947039191445,
 :expectancy-pips 9.648648648649075,
 :max-drawndown%  0.07920081066638286,
 :payoff-ratio    2.1504913877366363,
 :pips-loss       282.99999999999824,
 :pips-loss-avg   14.89473684210517,
 :pips-profit     357.0000000000157,
 :pips-win        640.000000000014,
 :pips-win-avg    35.55555555555633,
 :profit          7914.650404500835,
 :profit%         0.7914650404500835,
 :profit-factor   2.0373076304873394,
 :profitability   0.4864864864864865,
 :rr              0.41891447368419876,
 :sharpe-ratio    0.47048293064073926,
 :sortino-ratio   0.7779832161540341,
 :start-balance   10000,
 :trades-count    37,
 :trades-loss     19,
 :trades-win      18,
 :z-score         0.9015814224625143}


(count (comb/subsets [:zone-sp1 :zone-sp2 :zone-sc1 :zone-sc2 :zone-sc3 :zone-sc4]))

(def zone-analyse (atom []))
(async/go
  (reset! zone-analyse [])
  (let [vars (comb/subsets [:zone-sp1 :zone-sp2 :zone-sp3 :zone-sc1 :zone-sc2 :zone-sc3 :zone-sc4])]
    ;(correlation-analyse 1024)
    (analyse-quotes)
    (->> vars
         (rest)
         (mapv (fn [zone-types]
                 (timbre/warn (double (* 100 (/ (.indexOf vars zone-types) (count vars)))) "%")
                 (vtr-analyse zone-types)
                 (let [trades (all-trades dm/instruments dm/periods
                                          (first VTR) {:rr               3
                                                       :min-rr           nil
                                                       :profit-target    nil
                                                       :trailing-stop?   nil
                                                       :trailing-profit? nil
                                                       :break-even?      nil
                                                       :dcm?             false
                                                       :markets          [:frankfurt :london :new-york
                                                                          :tokyo :sydney :wellington]
                                                       :look-back        64})
                       result (assoc (stats/full {:trade-size-fn mm/redd-trade-size
                                                  :start-balance 10000
                                                  :max-risk      0.05
                                                  :max-dd        0.25}
                                                 trades)
                                :zone-types zone-types)]
                   (swap! zone-analyse conj result)))))))



(def look-back-analyse
  (let [vars [0 8 16 24 32 64 128 256 512]]
    (analyse-quotes)
    (correlation-analyse 1024)
    (->> vars
         (rest)
         (mapv (fn [look-back]
                 (timbre/warn (double (* 100 (/ (.indexOf vars look-back) (count vars)))) "%")
                 (vtr-analyse [:zone-sp1 :zone-sp2 :zone-sc1 :zone-sc2] look-back)
                 (let [trades (all-trades dm/instruments
                                          dm/periods
                                          VTR {:rr               10
                                               :profit-target    5
                                               :trailing-stop?   :wrb-hg
                                               :trailing-profit? :vtr
                                               :dcm?             true
                                               :markets          [:frankfurt :london :new-york :tokyo
                                                                  :hong-kong :sydney :wellington]})]
                   (assoc (stats/full {:trade-size-fn mm/redd-trade-size
                                       :start-balance 10000
                                       :max-risk      0.05
                                       :max-dd        0.25}
                                      trades)
                     :look-back look-back)))))))

(def correlation-ratio-analyse
  (let [vars (vec (range 0.70 1.0 0.05))]
    (->> vars
         (rest)
         (mapv (fn [ratio]
                 (timbre/warn (long (* 100 (/ (.indexOf vars ratio) (count vars)))) "%")
                 (analyse-quotes)
                 (vtr-analyse ratio [:zone-sc3] 128)
                 (let [trades (all-trades dm/instruments
                                          dm/periods
                                          wrb-vtr/VTR {:rr 10 :trailing? true :profit-target 3 :dcm? true})]
                   (assoc (stats/full trades
                                      {:start-balance 10000
                                       :max-risk      0.1
                                       :max-dd        0.25})
                     :correlation-ratio ratio)))))))
(count (comb/cartesian-product (range 2 6 1)
                               [nil]
                               [:wrb-hg nil]
                               [nil]
                               [1 2 3]
                               [false]
                               [nil 1 2 3]))

(def rr-analyse (atom []))
(do
  (reset! rr-analyse [])
  ;(download-quotes)
  ;(analyse-quotes)
  ;(fvb-analyse [:zone-sp1 :zone-sp2 :zone-sc1 :zone-sc2])
  ;(correlation-analyse 1024)
  ;(vtr-analyse [:zone-sp1])
  (let [vars (comb/cartesian-product (range 2 11 1)
                                     [nil]
                                     [nil]
                                     [nil]
                                     [nil]
                                     [false true]
                                     [nil])]
    (->> vars
         (map (fn [[rr min-rr trailing-stop? trailing-profit? break-even? dcm? profit-target]]
                (timbre/warn (double (* 100 (/ (.indexOf vars [rr min-rr trailing-stop? trailing-profit? break-even? dcm? profit-target]) (count vars)))) "%")
                (let [result (assoc (stats/descriptive
                                      (stats/add-money-management
                                        {:trade-size-fn mm/redd-trade-size
                                         :start-balance 10000
                                         :max-risk      0.03
                                         :max-dd        0.3
                                         :window        10}
                                        (->> (all-trades dm/instruments
                                                         [:m5 :m10]
                                                         (first VTR)
                                                         {:rr                 rr
                                                          :min-rr             min-rr
                                                          :profit-target      profit-target
                                                          :trailing-stop?     trailing-stop?
                                                          :trailing-profit?   trailing-profit?
                                                          :break-even?        break-even?
                                                          :dcm?               dcm?
                                                          :max-spread-percent 0.3
                                                          :markets            [:frankfurt :london :new-york
                                                                               :tokyo :sydney :wellington]})
                                             (distinct-by (juxt :period :open-time))
                                             (sort-by :open-time))))
                               :backtest-rr rr
                               :backtest-min-rr rr
                               :backtest-trailing-stop? trailing-stop?
                               :backtest-trailing-profit? trailing-profit?
                               :backtest-break-even? break-even?
                               :backtest-dcm? dcm?
                               :backtest-profit-target profit-target)]
                  (swap! rr-analyse conj result))))
         (doall))))

(def market-analyse (atom []))
(async/go
  (reset! market-analyse [])
  (let [vars [:frankfurt :london :new-york :tokyo :sydney :wellington :hong-kong]]
    (->> vars
         (pmap (fn [market]
                 (timbre/warn (double (* 100 (/ (.indexOf vars market) (count vars)))) "%")
                 (let [result (assoc (stats/full
                                       {:trade-size-fn mm/redd-trade-size
                                        :start-balance 10000
                                        :max-risk      0.05
                                        :max-dd        0.25}
                                       (all-trades dm/instruments dm/periods
                                                   FVB {:rr               3
                                                        :min-rr           nil
                                                        :profit-target    nil
                                                        :trailing-stop?   nil
                                                        :trailing-profit? nil
                                                        :break-even?      nil
                                                        :dcm?             false
                                                        :zones            [:zone-sp1 :zone-sp2 :zone-sp3]
                                                        :markets          [market]}))
                                :backtest-market market)]
                   (swap! market-analyse conj result))))
         (doall))))

(def mm-analyse (atom []))
(async/go
  (reset! mm-analyse [])
  (let [vars (vec (->> (comb/cartesian-product (range 0.01 0.11 0.01) (range 0.05 0.35 0.05)
                                               (range 1 11))
                       (filter (fn [[max-risk max-dd window]]
                                 (and (> max-dd max-risk) (<= (* max-risk window) max-dd))))))
        trades (all-trades dm/instruments
                           [:m5 :m10]
                           (first VTR)
                           {:rr                 3
                            :min-rr             nil
                            :trailing-stop?     nil
                            :trailing-profit?   nil
                            :break-even?        nil
                            :dcm?               nil
                            :profit-target      nil
                            :max-spread-percent 0.3
                            :markets            [:frankfurt :london :new-york :tokyo
                                                 :hong-kong :sydney :wellington]})]
    (->> vars
         (map (fn [[max-risk max-dd window]]
                (timbre/warn (double (* 100 (/ (.indexOf vars [max-risk max-dd window]) (count vars)))) "%")
                (let [result (assoc (stats/full {:trade-size-fn mm/redd-trade-size
                                                 :start-balance 10000
                                                 :max-risk      max-risk
                                                 :max-dd        max-dd
                                                 :window        window}
                                                trades)
                               :mm-max-risk max-risk
                               :mm-max-dd max-dd
                               :window window)]
                  (swap! mm-analyse conj result))))
         (doall)
         (remove nil?))))

(def super-analyse
  (let [vars (vec (comb/cartesian-product
                    (range 3 11)                            ;rr
                    [true false]                            ;trailing
                    [true false]                            ;dcm
                    [0 2 3]))]                              ;profit target
    ;traling?
    (->> vars
         (pmap (fn [[max-risk max-dd rr trailing? dcm? profit-target]]
                 (timbre/warn (double (* 100 (/ (.indexOf vars [max-risk max-dd rr trailing? dcm? profit-target])
                                                (count vars)))) "%")
                 (let [trades (all-trades dm/instruments
                                          dm/periods
                                          VTR {:rr            rr
                                               :trailing?     trailing?
                                               :dcm?          dcm?
                                               :profit-target profit-target})]
                   (assoc (stats/full {:trade-size-fn mm/redd-trade-size
                                       :start-balance 10000
                                       :max-risk      max-risk
                                       :max-dd        max-dd
                                       :window        5}
                                      trades)
                     :backtest-rr rr
                     :backtest-profit-target profit-target
                     :backtest-trailing? trailing?
                     :backtest-max-risk max-risk
                     :backtest-max-dd max-dd))))
         (doall)
         (remove nil?))))

(comb/count-subsets (vec dm/instruments))
(def instruments-analyse
  (let [vars (vec dm/instruments)]
    (->> dm/instruments
         (pmap (fn [instrument]
                 (timbre/warn (long (* 100 (/ (.indexOf vars instrument) (count vars)))) "%")
                 (assoc (stats/full
                          (all-trades [instrument]
                                      dm/periods
                                      VTR
                                      {:rr               5,
                                       :profit-target    3
                                       :trailing-stop?   :wrb-hg
                                       :trailing-profit? :vtr
                                       :break-even?      nil
                                       :dcm?             true
                                       :markets          [:frankfurt :london :new-york :tokyo
                                                          :hong-kong :sydney :wellington]})
                          {:trade-size-fn mm/redd-trade-size
                           :start-balance 10000
                           :max-risk      0.05
                           :max-dd        0.25})
                   :instrument instrument)))
         (doall))))



(count @rr-analyse)

(sort-by :profit% @rr-analyse)
(sort-by :profit% look-back-analyse)
(sort-by :sortino-ratio @rr-analyse)
(sort-by :profit% @mm-analyse)
(sort-by :profit% @market-analyse)
(sort-by :profit% correlation-ratio-analyse)
(->> @mm-analyse
     (filter #(>= (:window %) 3))
     (filter #(< (:max-drawndown% %) 0.3))
     (filter #(> (:profit% %) 1.0))
     (sort-by :sharpe-ratio))

(sort-by :profit-factor instruments-analyse)

(->> @rr-analyse
     ;(filter #(true? (:backtest-dcm? %)))
     (filter #(pos? (:profit %)))
     ;(filter #(< (:max-drawndown% %) 0.25))
     (filter #(< (:rr %) 0.5))
     (filter #(> (:profitability %) 0.2))
     (filter #(> (:profit% %) 0.3))
     (filter #(> (:backtest-rr %) 2))
     (sort-by :sharpe-ratio)
     (take-last 10))

(->> instruments-analyse
     (filter #(not (nil? (:profit-factor %))))
     (filter #(> (:sharpe-ratio %) 0))
     (sort-by :profitability)
     (reverse)
     (map :instrument))

:m5
{:backtest-break-even?      3,
 :backtest-dcm?             false,
 :backtest-min-rr           3,
 :backtest-profit-target    nil,
 :backtest-rr               3,
 :backtest-trailing-profit? nil,
 :backtest-trailing-stop?   nil,
 :cash-loss                 46487.5667097772,
 :cash-loss-avg             430.4404324979371,
 :cash-win                  67042.30904749433,
 :cash-win-avg              1314.555079362634,
 :duration-avg              15.584905660377366,
 :expectancy-cash           129.27510904224613,
 :expectancy-pips           3.2905660377358448,
 :max-drawndown%            0.21347969938704628,
 :mm-max-dd                 0.25,
 :mm-max-risk               0.05,
 :mm-start-balance          10000,
 :pips-loss                 962.7000000000054,
 :pips-loss-avg             8.91388888888894,
 :pips-profit               523.1999999999995,
 :pips-win                  1485.9000000000046,
 :pips-win-avg              29.13529411764715,
 :profit                    20554.74233771713,
 :profit%                   2.055474233771713,
 :profit-factor             1.4421556943610492,
 :profitability             0.3207547169811321,
 :rr                        0.30594813468829296,
 :sharpe-ratio              0.1506427245236828,
 :sortino-ratio             0.2779847474955937,
 :start-balance             10000,
 :trades-count              159,
 :trades-loss               108,
 :trades-win                51,
 :z-score                   0.6013712436573929}


(defn candlestick-chart [data start end & {:keys [title time-label value-label]}]
  (let [data (-> (q/subvector data start end)
                 (select-keys [:time# :open# :high# :low# :close#])
                 (q/columns->rows)
                 (->> (map (fn [{:keys [time#] :as candle}]
                             (assoc candle :time# (dt/minus time# (dt/hours 1))))))
                 (incanter.core/dataset))]
    (charts/candle-stick-plot :data (-> data
                                        (assoc :time
                                               (map #(dt/plus % (dt/hours 1)) (:time data))))
                              :date :time#
                              :open :open#
                              :high :high#
                              :low :low#
                              :close :close#
                              :title (or title "Candlestick Plot")
                              :time-label time-label
                              :value-label value-label)))

(download-quotes [:eur-usd] [:m15])
(analyse-quotes [:eur-usd] [:m15])
(let [data (db/get-quotes :eur-usd :m15)]
  (println (apply min (take 256 (:close# data))) (apply max (take 256 (:close# data))))
  (println (first (take 256 (:time# data))) (last (take 256 (:time# data))))
  (view (charts/line-chart (range 256) (reverse (take 256 (:close# data)))))
  (view (charts/line-chart (range 256) (reverse (take 256 (:wrb-dcm# data)))))
  (view (charts/line-chart (range 256) (reverse (take 256 (:wrb-dcm-trend# data))))))

(let [data (db/get-quotes :eur-usd :m15)]
  (map (juxt :time# :wrb-dcm-direction#) (filter #(not= 0 (:wrb-dcm-direction# %)) (q/row-maps data))))


(def trades (all-trades dm/instruments dm/periods
                        FVB {:rr               5
                             :min-rr           nil
                             :profit-target    nil
                             :trailing-stop?   :wrb-hg
                             :trailing-profit? :fvb
                             :break-even?      3
                             :dcm?             false
                             :markets          [:frankfurt :london :new-york
                                                :tokyo :sydney :wellington]}))

(first trades)

(defn show-trade [{:keys [instrument period open-candle-nr close-candle-nr] :as trade}]
  (let [data (db/get-quotes instrument period)
        zone-candle-nr (aget (:fvb-zone-nr# data) (inc open-candle-nr))]
    (view (candlestick-chart data (max 0 (- close-candle-nr 16)) (+ zone-candle-nr 16)))
    (assoc trade :zone-time (aget (:time# data) zone-candle-nr))))

(def counter (atom 0))
(let [trade (nth trades @counter)]
  (swap! counter inc)
  (show-trade trade))

(let [{:keys [instrument period open-candle-nr] :as trade} (nth trades 11)
      data (db/get-quotes instrument period)
      sister-instrument (aget (:vtr-sister-name data) (inc open-candle-nr))]
  (assoc trade
    :sister-name sister-instrument))

(let [{:keys [instrument period open-candle-nr] :as trade} (nth trades 8)
      data (db/get-quotes instrument period)
      prior-bear-hg (aget (:prior-unfilled-bear-hg data) (+ 2031 20))]
  [(aget (:time data) (- 2681 52))
   (aget (:wrb-hg-body# data) (- 2681 52))
   (aget (:time data) (aget (:prior-unfilled-bear-hg data) (- 2681 54)))])

(let [data (db/get-quotes :gbp-jpy :m10)]
  (let [r (->> data
               (q/row-maps)
               (map-indexed (fn [i r] (assoc r :i i)))
               (filterv #(not= 0 (:zone-sp1 %)))
               (mapv (fn [row] {:v1   (:i row)
                                :v2   (+ (:i row) (:zone-contraction row))
                                :dir  (:vsa-sd row)
                                :time (str (:time row))})))]
    ;:date-v2 (str (aget (:time data) (+ (:i row) (:zone-contraction row))))

    (def r r)
    r))

(let [data (db/get-quotes :us2000-usd :m15)]
  (->> data
       (q/row-maps)
       (map-indexed (fn [idx item]
                      (assoc item :idx idx)))
       (filter #(= (str (:time %)) "2015-11-18T15:30:00.000Z"))
       (first)
       :idx))
(let [data (db/get-quotes :usd-jpy :m15)]
  (let [r (->> data
               (q/row-maps)
               (map-indexed (fn [i r] (assoc r :i i)))
               (filterv #(not= 0 (:vsa-sd %)))
               (mapv (fn [row] {:i    (:i row)
                                :dir  (:vsa-sd row)
                                :date (str (:time row))})))]
    (def r r)
    r))

(let [data (db/get-quotes :gbp-jpy :m10)]
  (aget (:time data) 0))


(let [n 0
      data (db/get-quotes :eur-usd :m10)
      i (:i (nth r n))
      l 4
      d 50]
  (println (nth r n))
  (-> (candlestick-chart data
                         (- i (max l d)) (+ i (max l d)))
      (view)))


(let [instrument :usd-jpy
      period :m15
      data (db/get-quotes instrument period)
      i 45
      l 10
      d 50]
  (println (:time (q/get-row data i)))
  (-> (candlestick-chart data
                         (- i (min i (max l d))) (+ i (max l d))
                         :title (str (name instrument) "_" (name period))
                         :time-label (str "time open " (:time (q/get-row data i)))
                         :value-label (str "price open " (:open (q/get-row data i))))
      (view)))

(let [instrument :eur-usd
      period :m15
      data (db/get-quotes instrument period)
      data (sd/base data 3 6)
      data (q/row-maps data)]
  (->> data
       (filter (comp true? :sd-base#))
       (map #(select-keys % [:time# :sd-base# :sd-base-count#]))
       (take 10)))

(->> [1 2 3 4]
     (filter (comp odd?)))


(defn save-trades-charts [trades]
  (doseq [{:keys [i instrument period open-price open-time close-time] :as trade} trades]
    (let [data (db/get-quotes instrument period)
          signal-candle (->> data
                             (q/row-maps)
                             (map-indexed (fn [i r] (assoc r :i i)))
                             (filterv #(= (+ i 2) (:i %)))
                             (first))]
      (candlestick-chart data
                         (max 0 (- (:inside-zone-nr signal-candle) 10))
                         (+ i (max (:length trade) 25))
                         :title (str (name instrument) "_" (name period))
                         :time-label (str "time open " open-time)
                         :volume-label (str "price open " open-price)))))


(view (candlestick-chart (db/get-quotes :eur-usd :m5)
                         0 10))
