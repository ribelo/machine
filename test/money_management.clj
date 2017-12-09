(ns money-management
  (:require [clojure.test :refer :all]
            [machine.money-management.core :refer :all]))



(deftest test-mm
  (is (= [1.0 2.0 3.0] (profit-history [{:profit 1.0 :test1 "test1"}
                                        {:profit 2.0 :test2 100}
                                        {:profit 3.0 :test3 :test3}])))
  (is (= [1.0 2.0 3.0] (pips-history [{:pips-profit 1.0 :test1 "test1"}
                                      {:pips-profit 2.0 :test2 100}
                                      {:pips-profit 3.0 :test3 :test3}])))
  (is (= [1.0 2.0 3.0] (balance-history [{:account-balance 1.0 :test1 "test1"}
                                         {:account-balance 2.0 :test2 100}
                                         {:account-balance 3.0 :test3 :test3}])))
  (is (nil? (highest-equity [])))
  (is (= 700.0 (highest-equity [{:account-balance 700.0 :test1 "test1"}
                                {:account-balance 600.0 :test2 100}
                                {:account-balance 500.0 :test3 :test3}
                                {:account-balance 400.0}
                                {:account-balance 300.0}
                                {:account-balance 200.0}
                                {:account-balance 100.0}])))
  (is (= 200.0 (highest-equity [{:account-balance 700.0 :test1 "test1"}
                                {:account-balance 600.0 :test2 100}
                                {:account-balance 500.0 :test3 :test3}
                                {:account-balance 400.0}
                                {:account-balance 300.0}
                                {:account-balance 200.0}
                                {:account-balance 100.0}] 2)))
  (is (= 500.0 (highest-equity [{:account-balance 700.0 :test1 "test1"}
                                {:account-balance 600.0 :test2 100}
                                {:account-balance 500.0 :test3 :test3}
                                {:account-balance 400.0}
                                {:account-balance 300.0}
                                {:account-balance 200.0}
                                {:account-balance 100.0}] 5)))
  (is (= 100.0 (lowest-equity [{:account-balance 700.0 :test1 "test1"}
                               {:account-balance 600.0 :test2 100}
                               {:account-balance 500.0 :test3 :test3}
                               {:account-balance 400.0}
                               {:account-balance 300.0}
                               {:account-balance 200.0}
                               {:account-balance 100.0}])))
  (is (= 100.0 (lowest-equity [{:account-balance 700.0 :test1 "test1"}
                               {:account-balance 600.0 :test2 100}
                               {:account-balance 500.0 :test3 :test3}
                               {:account-balance 400.0}
                               {:account-balance 300.0}
                               {:account-balance 200.0}
                               {:account-balance 100.0}] 2)))
  (is (= 100.0 (lowest-equity [{:account-balance 700.0 :test1 "test1"}
                               {:account-balance 600.0 :test2 100}
                               {:account-balance 500.0 :test3 :test3}
                               {:account-balance 400.0}
                               {:account-balance 300.0}
                               {:account-balance 200.0}
                               {:account-balance 100.0}] 5)))
  (is (nil? (reward-average [])))
  (is (= 20.0 (reward-average [{:profit 10} {:profit 30} {:profit -10}])))
  (is (nil? (pips-reward-average [])))
  (is (= 20.0 (pips-reward-average [{:pips-profit 10} {:pips-profit 30} {:pips-profit -10}])))
  (is (nil? (risk-average [])))
  (is (= -20.0 (risk-average [{:profit -10} {:profit -30} {:profit 10}])))
  (is (nil? (pips-risk-average [])))
  (is (= -20.0 (pips-risk-average [{:pips-profit -10} {:pips-profit -30} {:pips-profit 10}])))
  (is (nil? (risk-reward [])))
  (is (= 0.25 (risk-reward [{:profit 10} {:profit 30} {:profit -10}])))
  (is (nil? (profitability [])))
  (is (= 0.5 (profitability [{:profit 10} {:profit 30} {:profit -10} {:profit -10}]))))
