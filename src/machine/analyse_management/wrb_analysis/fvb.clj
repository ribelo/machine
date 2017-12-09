(ns machine.analyse-management.wrb-analysis.fvb
  (:require [machine.quotes.series :refer :all]
            [machine.analyse-management.wrb-analysis.utils :refer :all]
            [criterium.core :refer [bench quick-bench]])
  (:import [machine.quotes.core Quotes]))


(defprotocol Fade-Volatility-Breakout
  (fade-volatility-breakout [data zone-types] [data zone-types i]))


(extend-type Quotes
  Fade-Volatility-Breakout
  (fade-volatility-breakout
    (^Quotes [^Quotes {:keys [^objects time# ^doubles high# ^doubles low# ^doubles close# ^longs candle-direction#
                              ^doubles candle-body-size# ^doubles candle-body-midpoint#
                              ^longs candle-broken-range# ^longs wrb-body# ^longs wrb-hg-body#]
                       :as   data} zone-types]
     (let [array-size (alength time#)
           zone# (identity-union (mapv #(get data (zone-keyword %)) zone-types))
           zone-open# (price-union (mapv #(get data (open-keyword %)) zone-types))
           zone-close# (price-union (mapv #(get data (close-keyword %)) zone-types))
           zone-size# (price-union (mapv #(get data (size-keyword %)) zone-types))
           zone-filled-by# (identity-union (mapv #(get data (filled-by-keyword %)) zone-types))
           zone-contraction-size# (price-union (mapv #(get data (contraction-size-keyword %)) zone-types))
           zone-confirmation-candle# (identity-union (mapv #(get data (confirmation-candle-keyword %)) zone-types))
           zone-type# (object-union (mapv #(get data (type-keyword %)) zone-types))
           fvb# (long-array array-size)
           fvb-zone-nr# (long-array array-size)
           zone-used-by# (long-array array-size)]
       (loop [i (- array-size 3)]
         (when (>= i 0)
           (condp = (aget candle-direction# i)
             1 (when (and (== -1 (aget wrb-body# (inc i)))
                          (>= (aget close# i) (aget candle-body-midpoint# (inc i)))
                          (>= (aget candle-broken-range# (inc i)) 3))

                 (loop [j 3]
                   (when (and (< j (- array-size i 1)) (<= j 64))
                     (if (and (== 1 (aget zone# (+ i j)))
                              ;(or (and (= :swing-point (aget zone-type# (+ i j))) (<= 16 j 64))
                              ;    (and (= :strong-continuation (aget zone-type# (+ i j))) (<= j 16)))
                              (zero? (aget zone-used-by# (+ i j)))
                              (>= (aget zone-confirmation-candle# (+ i j)) i)
                              ; zone must have body size greater than c1 candle
                              (> (aget zone-size# (+ i j))
                                 (aget candle-body-size# (inc i)))
                              ; zone must be unfilled
                              (<= (aget zone-filled-by# (+ i j)) (inc i))

                              (or
                                (and (< (aget zone-open# (+ i j))
                                        (aget close# (inc i))
                                        (aget zone-close# (+ i j)))
                                     ;(> (amin (aslice close# (+ (inc i) 1) (+ (inc i) 4)))
                                     ;   (aget zone-close# (+ i j)))
                                     (> (aget close# i)
                                        (aget zone-close# (+ i j))
                                        (aget zone-open# (+ i j))))
                                (and (< (aget close# (inc i))
                                        (aget zone-open# (+ i j))
                                        (aget zone-close# (+ i j)))
                                     (> (aget zone-close# (+ i j))
                                        (aget close# i)
                                        (aget zone-open# (+ i j))))))

                       (do
                         (aset fvb# i 1)
                         (aset fvb-zone-nr# i (+ i j))
                         (aset zone-used-by# (+ i j) i))
                       (recur (inc j))))))
             -1 (when (and (== 1 (aget wrb-body# (inc i)))
                           (<= (aget close# i) (aget candle-body-midpoint# (inc i)))
                           (>= (aget candle-broken-range# (inc i)) 3))

                  (loop [j 3]
                    (when (and (< j (- array-size i 1)) (<= j 64))
                      (if (and (== -1 (aget zone# (+ i j)))
                               ;(or (and (= :swing-point (aget zone-type# (+ i j))) (<= 16 j 64))
                               ;    (and (= :strong-continuation (aget zone-type# (+ i j))) (<= j 16)))
                               (zero? (aget zone-used-by# (+ i j)))
                               (>= (aget zone-confirmation-candle# (+ i j)) i)
                               ; zone must have body size greater than c1 candle
                               (> (aget zone-size# (+ i j))
                                  (aget candle-body-size# (inc i)))
                               ; zone must be unfilled
                               (<= (aget zone-filled-by# (+ i j)) (inc i))

                               (or
                                 (and (> (aget zone-open# (+ i j))
                                         (aget close# (inc i))
                                         (aget zone-close# (+ i j)))
                                      ;(< (amax (aslice close# (+ (inc i) 1) (+ (inc i) 4)))
                                      ;   (aget zone-close# (+ i j)))
                                      (< (aget close# i)
                                         (aget zone-close# (+ i j))
                                         (aget zone-open# (+ i j))))
                                 (and (> (aget close# (inc i))
                                         (aget zone-open# (+ i j))
                                         (aget zone-close# (+ i j)))
                                      (< (aget zone-close# (+ i j))
                                         (aget close# i)
                                         (aget zone-open# (+ i j))))))

                        (do
                          (aset fvb# i -1)
                          (aset fvb-zone-nr# i (+ i j))
                          (aset zone-used-by# (+ i j) i))
                        (recur (inc j))))))
             0 nil)
           (recur (dec i))))
       (-> data
           (assoc :fvb# fvb#)
           (assoc :fvb-zone-nr# fvb-zone-nr#))))
    ([{:keys [^longs fvb# ^longs fvb-zone-nr#] :as data}
      zone-types ^long ^long i]
     (if fvb#
       {:fvb#         (aget fvb# i)
        :fvb-zone-nr# (aget fvb-zone-nr# i)}
       (recur (fade-volatility-breakout data zone-types) zone-types i)))))
