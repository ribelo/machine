(ns machine.money-management.core
  (:require [schema.core :as s]
            [mount.core :refer [defstate]]
            [taoensso.timbre :as timbre]
            [machine.oanda.core :as oanda]
            [machine.utils :refer :all]
            [machine.oanda.schema :refer :all]
            [machine.settings.core :refer [settings]]
            [machine.money-management.schema :refer :all])
  (:import [org.apache.commons.math3.util FastMath]
           [machine.oanda.core OandaAccount]))


(s/defn profit-history :- (s/maybe [s/Num])
  [transactions-closed :- [{:profit s/Num s/Keyword s/Any}]]
  (when (seq transactions-closed)
    (->> transactions-closed
         (map :profit)
         (remove nil?)
         (mapv double))))


(s/defn pips-history :- (s/maybe [s/Num])
  [transactions-closed :- [{:pips-profit s/Num s/Keyword s/Any}]]
  (when (seq transactions-closed)
    (->> transactions-closed
         (map :pips-profit)
         (remove nil?)
         (mapv double))))


(s/defn balance-history :- (s/maybe [s/Num])
  [transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}]]
  (let [{:keys [account-balance profit]} (last transactions-closed)
        history (->> transactions-closed
                     (sort-by :time)
                     (map :account-balance)
                     (remove nil?)
                     (mapv double))]
    (if (and account-balance profit)
      (conj history (+ account-balance profit))
      history)))


(s/defn highest-equity :- (s/maybe s/Num)
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}] n :- (s/maybe s/Num)]
    (if-let [history (cond->> (balance-history transactions-closed)
                              n (take-last n))]
      (when (seq history) (apply max history))))
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}]]
    (highest-equity transactions-closed nil)))


(s/defn lowest-equity :- (s/maybe s/Num)
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}] n :- (s/maybe s/Num)]
    (if-let [history (cond->> (balance-history transactions-closed)
                              n (take-last n))]
      (when (seq history) (apply min history))))
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}]]
    (lowest-equity transactions-closed nil)))


(s/defn reward-average :- (s/maybe s/Num)
  ([transactions-closed :- [{:profit s/Num s/Keyword s/Any}] n :- (s/maybe s/Num)]
    (let [trades (cond->> (profit-history transactions-closed)
                          n (take-last n))
          win-trades (filter pos? trades)]
      (when (seq win-trades)
        (/ (reduce + win-trades) (count win-trades)))))
  ([transactions-closed :- [{:profit s/Num s/Keyword s/Any}]]
    (reward-average transactions-closed nil)))


(s/defn pips-reward-average :- (s/maybe s/Num)
  [transactions-closed :- [{:pips-profit (s/maybe s/Num) s/Keyword s/Any}]]
  (let [trades (pips-history transactions-closed)
        win-trades (filter pos? trades)]
    (when (seq win-trades)
      (/ (reduce + win-trades) (count win-trades)))))


(s/defn risk-average :- (s/maybe s/Num)
  [transactions-closed :- [{:profit s/Num s/Keyword s/Any}]]
  (let [trades (profit-history transactions-closed)
        loss-trades (filter neg? trades)]
    (when (seq loss-trades)
      (/ (reduce + loss-trades) (count loss-trades)))))


(s/defn pips-risk-average :- (s/maybe s/Num)
  [transactions-closed :- [{:pips-profit s/Num s/Keyword s/Any}]]
  (let [trades (pips-history transactions-closed)
        loss-trades (filter neg? trades)]
    (when (seq loss-trades)
      (/ (reduce + loss-trades) (count loss-trades)))))


(s/defn risk-reward :- (s/maybe s/Num)
  [transactions-closed :- [{:profit s/Num s/Keyword s/Any}]]
  (let [trades (profit-history transactions-closed)
        win (FastMath/abs ^double (reduce + (filter pos? trades)))
        loss (FastMath/abs ^double (reduce + (filter neg? trades)))]
    (when (every? pos? [win loss])
      (/ loss win))))


(s/defn pips-risk-reward :- (s/maybe s/Num)
  [transactions-closed :- [{:pips-profit s/Num s/Keyword s/Any}]]
  (let [trades (pips-history transactions-closed)
        win (FastMath/abs ^double (reduce + (filter pos? trades)))
        loss (FastMath/abs ^double (reduce + (filter neg? trades)))]
    (when (every? pos? [win loss])
      (/ loss win))))


(s/defn profitability :- (s/maybe s/Num)
  ([transactions-closed :- [{:profit s/Num s/Keyword s/Any}] n :- (s/maybe s/Num)]
    (let [history (cond->> (profit-history transactions-closed)
                           n (take-last n))
          win-trades (filter pos? history)]
      (when (pos? (count win-trades))
        (double (/ (count win-trades) (count history))))))
  ([transactions-closed :- [{:profit s/Num s/Keyword s/Any}]]
    (profitability transactions-closed nil)))


(s/defn max-drawndown :- s/Num
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}] n :- (s/maybe s/Num)]
    (let [history (cond->> (balance-history transactions-closed)
                           n (take-last n))]
      (if (seq history)
        (->> history
             (partition-between (fn [a b] (> b a)))
             (remove (fn [coll] (> (last coll) (first coll))))
             (map (fn [coll] (- (last coll) (first coll))))
             (apply min)
             (double)
             (FastMath/abs))
        0.0)))
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}]]
    (max-drawndown transactions-closed nil)))


(s/defn max-peak-valey-drawndown-percent :- s/Num
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}] n :- (s/maybe s/Num)]
    (let [history (cond->> (balance-history transactions-closed)
                           n (take-last n))]
      (if (seq history)
        (let [dd (->> history
                      (partition-between (fn [a b] (> b a)))
                      (remove (fn [coll] (> (last coll) (first coll))))
                      (map (fn [coll] (/ (- (first coll) (last coll)) (first coll)))))]
          (when (seq dd) (->> (apply max dd) (double))))
        0.0)))
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}]]
    (max-peak-valey-drawndown-percent transactions-closed nil)))


(s/defn max-eceonomic-drawndown-percent :- s/Num
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}] n :- (s/maybe s/Num)]
    (let [[last-balance last-profit] ((juxt :account-balance :profit) (last transactions-closed))]
      (if (and last-balance last-profit)
        (let [balance (+ last-balance last-profit)
              highest-eq (highest-equity transactions-closed n)]
          (if (and balance highest-eq)
            (/ (- highest-eq balance) highest-eq)
            0.0))
        0.0)))
  ([transactions-closed :- [{:account-balance s/Num s/Keyword s/Any}]]
    (max-eceonomic-drawndown-percent transactions-closed nil)))


(s/defn pips-drawndown
  [transactions-closed :- [{:pips-profit s/Num s/Keyword s/Any}] count :- s/Num]
  (loop [dd 0.0 max-dd 0.0 [profit & r] (->> (pips-history transactions-closed)
                                             (take-last count))]
    (if-not (nil? profit)
      (if (neg? profit)
        (recur (- dd (FastMath/abs ^double profit)) max-dd r)
        (recur 0.0 (Math/min dd max-dd) r))
      (FastMath/abs (Math/min dd max-dd)))))


(s/defn percent-trade-size :- (s/maybe s/Num)
  [{:keys [account instrument open-price stop-loss-price balance
           max-risk]} transaction-closed]
  (let [stop-pips (/ (FastMath/abs ^double (- open-price stop-loss-price))
                     (oanda/instrument-pip account instrument))]
    (when (pos? stop-pips)
      (let [risk-units (-> balance
                           (* max-risk)
                           (/ stop-pips)
                           (/ (oanda/instrument-tick-value account instrument))
                           (FastMath/ceil))]
        (FastMath/ceil risk-units)))))


(s/defn redd-trade-size :- (s/maybe long)
  ([{:keys [account instrument open-price stop-loss-price balance
            max-risk max-dd window]} :- {:account                 OandaAccount
                                         :instrument              s/Keyword
                                         :open-price              s/Num
                                         :stop-loss-price         s/Num
                                         :balance                 s/Num
                                         :max-risk                s/Num
                                         :max-dd                  s/Num
                                         (s/optional-key :window) (s/maybe s/Num)
                                         s/Any                    s/Any}
    transaction-closed :- [{:account-balance s/Num s/Keyword s/Any}]]
    (timbre/info "[redd-trade-size] -" [instrument])
    (timbre/info "[redd-trade-size] -" [instrument] "- open-price" open-price)
    (timbre/info "[redd-trade-size] -" [instrument] "- stop-loss-price" stop-loss-price)
    (timbre/info "[redd-trade-size] -" [instrument] "- balance" balance)
    (timbre/info "[redd-trade-size] -" [instrument] "- max-risk" max-risk)
    (timbre/info "[redd-trade-size] -" [instrument] "- max-dd" max-dd)
    (let [stop-pips (/ (FastMath/abs ^double (- open-price stop-loss-price))
                       (oanda/instrument-pip account instrument))]
      (when (pos? stop-pips)
        (let [percent-dd (max-eceonomic-drawndown-percent transaction-closed (or window 5))
              risk-units (-> balance
                             (* max-risk)
                             (/ stop-pips)
                             (/ (oanda/instrument-tick-value account instrument))
                             (FastMath/floor)
                             (long))
              redd-units (FastMath/floor (* risk-units (- 1 (/ percent-dd max-dd))))]
          (timbre/info "[redd-trade-size] -" [instrument] "- actual-dd" percent-dd)
          (timbre/info "[redd-trade-size] -" [instrument] "- highest-eq" (highest-equity transaction-closed))
          (timbre/info "[redd-trade-size] -" [instrument] "- units" redd-units)
          (timbre/info "[redd-trade-size] -" [instrument] "result-risk"
                       (/ (* (oanda/instrument-tick-value account instrument)
                             stop-pips redd-units)
                          balance))
          (when (pos? redd-units)
            (long (min redd-units
                       (oanda/instrument-max-trade-units account instrument)
                       (oanda/instrument-units-available account instrument balance))))))))
  ([{:keys [account] :as opts}]
    (redd-trade-size opts (oanda/transactions-closed account))))

(/ (- 0.3 0.01)
   (- 1 0.01))