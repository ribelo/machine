(ns machine.analyse-management.quantum.correlation
  (:require [machine.quotes.series :refer :all])
  (:import [org.apache.commons.math3.stat.correlation PearsonsCorrelation]
           [machine.quotes.core Quotes]
           [org.apache.commons.math3.util FastMath]))


(defn pearsons-correlation
  ^Quotes [^Quotes main-data ^Quotes sister-data length]
  (let [^doubles main-close# (:close# main-data)
        ^doubles sister-close# (:close# sister-data)
        loop-limit (FastMath/min (alength main-close#) (alength sister-close#))]
    (if (= (aget ^objects (:time# main-data) 0)
           (aget ^objects (:time# sister-data) 0))
      (if length
        (let [^doubles result# (double-array (alength main-close#))]
          (loop [i (dec (- loop-limit length))]
            (when (>= i 0)
              (let [correlation
                    (.correlation (PearsonsCorrelation.)
                                  (aslice main-close# i (+ i length))
                                  (aslice sister-close# i (+ i length)))]
                (aset result# i correlation))
              (recur (dec i))))
          (assoc main-data
            (keyword (str "correlation-" (name (:instrument sister-data)) "#"))
            result#))
        (let [n (FastMath/min (alength main-close#)
                              (alength sister-close#))
              correlation (.correlation (PearsonsCorrelation.)
                                        (atake main-close# n)
                                        (atake sister-close# n))
              result# (double-array (repeat (alength main-close#) correlation))]
          (assoc main-data
            (keyword (str "correlation-" (name (:instrument sister-data)) "#"))
            result#)))
      (assoc main-data
        (keyword (str "correlation-" (name (:instrument sister-data)) "#"))
        (double-array (alength main-close#))))))
