(ns machine.analyse-management.utils
  (:require [schema.core :as s]
            [machine.quotes.series :refer :all])
  (:import [org.apache.commons.math3.stat.correlation PearsonsCorrelation]
           [machine.quotes.core Quotes]
           [org.apache.commons.math3.util FastMath]))


(s/defn correlation :- double
  [main-data :- Quotes sister-data :- Quotes]
  (if (= (aget ^objects (:time main-data) 0)
           (aget ^objects (:time sister-data) 0))
    (let [^doubles main-close (:close main-data)
          ^doubles sister-close (:close sister-data)
          n (FastMath/min (int 1024)
                          ^int (FastMath/min (alength main-close)
                                        (alength sister-close)))]
      (.correlation (PearsonsCorrelation.)
                    (atake (:close main-data) n)
                    (atake (:close sister-data) n)))
    0.0))
