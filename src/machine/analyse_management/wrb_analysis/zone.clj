(ns machine.analyse-management.wrb-analysis.zone
  (:require [machine.quotes.series :refer :all]
            [machine.analyse-management.wrb-analysis.utils :refer :all]
            [machine.analyse-management.wrb-analysis.expand :refer :all]
            [criterium.core :refer [bench quick-bench]])
  (:import [machine.quotes.core Quotes]
           [org.apache.commons.math3.util FastMath]))


(set! *unchecked-math* true)


(defprotocol Zone
  (swing-point-1 [data contraction-size] [data contraction-size i])
  (swing-point-2 [data contraction-size] [data contraction-size i])
  (swing-point-3 [data] [data i])
  (strong-continuation-1 [data contraction-size] [data contraction-size i])
  (strong-continuation-2 [data contraction-size] [data contraction-size i])
  (strong-continuation-3 [data contraction-size] [data contraction-size i])
  (strong-continuation-4 [data contraction-size] [data contraction-size i]))

(extend-type Quotes
  Zone
  (swing-point-1
    (^Quotes [^Quotes {:keys [^objects time# ^doubles open# ^doubles high# ^doubles low# ^doubles close#
                              ^longs candle-direction# ^longs candle-broken-range#
                              ^longs candle-filled-by# ^longs wrb-hg-body#
                              ^longs prior-unfilled-bull-wrb-hg# ^longs prior-unfilled-bear-wrb-hg#]
                       :as   data} ^long contraction-size]
     (let [array-size (alength time#)
           loop-limit (- array-size 3)
           zone# (long-array array-size)
           zone-open# (double-array array-size)
           zone-close# (double-array array-size)
           zone-contraction-count# (long-array array-size)
           zone-contraction-size# (double-array array-size)
           zone-confirmation-candle# (long-array array-size)
           zone-size# (double-array array-size)
           zone-filled-by# (long-array array-size)
           zone-type# (object-array array-size)]
       (loop [v1 4]
         (when (< v1 loop-limit)
           (case (aget wrb-hg-body# v1)
             1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                 (loop [j 4]
                   (when (< j loop-limit)
                     (let [v2 (+ v1 j)]
                       (if (and
                             ;V2 must be wrb
                             (not= 0 (aget wrb-hg-body# v2))
                             ;V2 shuld be breakout interval
                             (>= (aget candle-broken-range# v2) 3)
                             ;V2 shuld be unfilled until v1
                             (<= (aget candle-filled-by# v2) v1)
                             ;Contraction volatility must share min 1 pip with V2
                             (contraction-share? v1 v2 data)
                             ;V1 must breakout volatility contraction and
                             ;Body of V1/V2 must be greater than volatility contraction
                             (contraction-break-bull? v1 v2 data)
                             ;V1 must produce min 2 consecutive bull candles close# above
                             (bull-expand-after v1 8 data))
                         (let [prior-hg (aget prior-unfilled-bear-wrb-hg# v2)]
                           (when (and (pos? prior-hg)
                                      (< (FastMath/max 0 (- v1 8)) (aget candle-filled-by# prior-hg) v1)
                                      (or (== -1 (aget candle-direction# v2))
                                          (and
                                            (== 1 (aget candle-direction# v2))
                                            (let [swing-point (if (< (aget low# v2) (aget low# (inc v2)))
                                                                v2 (inc v2))]
                                              (<= (aget low# swing-point)
                                                  (amin (aslice low# (inc swing-point) prior-hg)))))))
                             (do
                               (aset zone# v1 (aget candle-direction# v1))
                               (aset zone-open# v1 (aget open# v1))
                               (aset zone-close# v1 (aget close# v1))
                               (aset zone-contraction-count# v1 j)
                               (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                                  (amin (aslice low# (inc v1) v2))))
                               (aset zone-confirmation-candle# v1 (FastMath/min ^long (bull-expand-after v1 8 data)
                                                                                (aget candle-filled-by# prior-hg)))
                               (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                               (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                               (aset zone-type# v1 :swing-point))))

                         (recur (inc j)))))))
             -1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                  (loop [j 4]
                    (when (< j loop-limit)
                      (let [v2 (+ v1 j)]
                        (if (and
                              ;V2 must be wrb
                              (not= 0 (aget wrb-hg-body# v2))
                              ;V2 shuld be breakout interval
                              (>= (aget candle-broken-range# v2) 3)
                              ;V2 shuld be unfilled until v1
                              (<= (aget candle-filled-by# v2) v1)
                              ;Contraction volatility must share min 1 pip with V2
                              (contraction-share? v1 v2 data)
                              ;V1 must breakout volatility contraction and
                              ;Body of V1/V2 must be greater than volatility contraction
                              (contraction-break-bear? v1 v2 data)
                              ;V1 must produce min 2 consecutive bear candles close# above
                              (bear-expand-after v1 8 data))
                          (let [prior-hg (aget prior-unfilled-bull-wrb-hg# v2)]
                            (when (and (pos? prior-hg)
                                       (< (FastMath/max 0 (- v1 8)) (aget candle-filled-by# prior-hg) v1)
                                       (or (== 1 (aget candle-direction# v2))
                                           (and
                                             (== -1 (aget candle-direction# v2))
                                             (let [swing-point (if (> (aget high# v2) (aget high# (inc v2)))
                                                                 v2 (inc v2))]
                                               (>= (aget high# swing-point)
                                                   (amax (aslice high# (inc swing-point) prior-hg)))))))
                              (do
                                (aset zone# v1 (aget candle-direction# v1))
                                (aset zone-open# v1 (aget open# v1))
                                (aset zone-close# v1 (aget close# v1))
                                (aset zone-contraction-count# v1 j)
                                (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                                   (amin (aslice low# (inc v1) v2))))
                                (aset zone-confirmation-candle# v1 (FastMath/min ^long (bear-expand-after v1 8 data)
                                                                                 (aget candle-filled-by# prior-hg)))
                                (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                                (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                                (aset zone-type# v1 :swing-point))))

                          (recur (inc j)))))))
             0 nil)
           (recur (inc v1))))
       (assoc data :zone-sp1# zone#
                   :zone-sp1-open# zone-open#
                   :zone-sp1-close# zone-close#
                   :zone-sp1-contraction-count# zone-contraction-count#
                   :zone-sp1-contraction-size# zone-contraction-size#
                   :zone-sp1-confirmation-candle# zone-confirmation-candle#
                   :zone-sp1-size# zone-size#
                   :zone-sp1-filled-by# zone-filled-by#
                   :zone-sp1-type# zone-type#)))
    ([{:keys [^longs zone-sp1# ^doubles zone-sp1-open# ^doubles zone-sp1-close#
              ^longs zone-sp1-contraction-count# ^doubles zone-sp1-contraction-size#
              ^longs zone-sp1-confirmation-candle#
              ^doubles zone-sp1-size# ^longs zone-sp1-filled-by#
              ^objects zone-sp1-type#] :as data} ^long contraction-size ^long i]
     (if zone-sp1#
       {:zone-sp1#                     (aget zone-sp1# i)
        :zone-sp1-open#                (aget zone-sp1-open# i)
        :zone-sp1-close#               (aget zone-sp1-close# i)
        :zone-sp1-contraction-count#   (aget zone-sp1-contraction-count# i)
        :zone-sp1-contraction-size#    (aget zone-sp1-contraction-size# i)
        :zone-sp1-confirmation-candle# (aget zone-sp1-confirmation-candle# i)
        :zone-sp1-size#                (aget zone-sp1-size# i)
        :zone-sp1-filled-by#           (aget zone-sp1-filled-by# i)
        :zone-sp1-type#                (aget zone-sp1-type# i)}
       (recur (swing-point-1 data contraction-size) contraction-size i))))
  (swing-point-2
    (^Quotes [^Quotes {:keys [^objects time# ^doubles open# ^doubles high# ^doubles low#
                              ^doubles close# ^longs candle-direction#
                              ^longs candle-consecutive-direction#
                              ^longs candle-broken-range#
                              ^longs candle-filled-by# ^longs wrb-hg-body#]
                       :as   data} ^long contraction-size]
     (let [array-size (alength time#)
           zone# (long-array array-size)
           zone-open# (double-array array-size)
           zone-close# (double-array array-size)
           zone-contraction-count# (long-array array-size)
           zone-contraction-size# (double-array array-size)
           zone-confirmation-candle# (long-array array-size)
           zone-size# (double-array array-size)
           zone-filled-by# (long-array array-size)
           zone-type# (object-array array-size)]
       (loop [v1 2]
         (when (< v1 (- array-size 3))
           (case (aget wrb-hg-body# v1)
             1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                 (loop [j 4]
                   (when (< j loop-limit)
                     (let [v2 (+ v1 j)]
                       (if (and
                             ;V1 must be bull
                             ;V2 must be wrb
                             (== -1 (aget wrb-hg-body# v2))
                             ;V2 shuld be unfilled until v1
                             (<= (aget candle-filled-by# v2) v1)
                             ;V2 shuld be breakout interval
                             (>= (aget candle-broken-range# v2) 3)
                             ;produce min 3 consecutive bull candles close# above v1
                             ;(bull-consecutive-direction v2 3 8 data)
                             (bull-expand-after v1 8 data)
                             ;Contraction volatility must share min 1 pip with V2
                             (contraction-share? v1 v2 data)
                             ;V1 must breakout volatility contraction and
                             ;Body of V1/V2 must be greater than volatility
                             ;contraction
                             (contraction-break-bull? v1 v2 data))
                         (do
                           (aset zone# v1 (aget candle-direction# v1))
                           (aset zone-open# v1 (aget open# v1))
                           (aset zone-close# v1 (aget close# v1))
                           (aset zone-contraction-count# v1 j)
                           (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                              (amin (aslice low# (inc v1) v2))))
                           (aset zone-confirmation-candle# v1 (bull-expand-after v1 8 data))
                           (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                           (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                           (aset zone-type# v1 :swing-point)))
                       (recur (inc j))))))
             -1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                  (loop [j 4]
                    (when (< j loop-limit)
                      (let [v2 (+ v1 j)]
                        (if (and
                              ;V1 must be bull
                              ;V2 must be wrb
                              (== 1 (aget wrb-hg-body# v2))
                              ;V2 shuld be unfilled until v1
                              (<= (aget candle-filled-by# v2) v1)
                              ;V2 shuld be breakout interval
                              (>= (aget candle-broken-range# v2) 3)
                              ;must produce min 3 consecutive bear candles close# belove v1
                              ;(bear-consecutive-direction v2 3 8 data)
                              (bear-expand-after v1 8 data)
                              ;Contraction volatility must share min 1 pip with V2
                              (contraction-share? v1 v2 data)
                              ;V1 must breakout volatility contraction and
                              ;Body of V1/V2 must be greater than volatility
                              ;contraction
                              (contraction-break-bear? v1 v2 data))
                          (do
                            (aset zone# v1 (aget candle-direction# v1))
                            (aset zone-open# v1 (aget open# v1))
                            (aset zone-close# v1 (aget close# v1))
                            (aset zone-contraction-count# v1 j)
                            (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                               (amin (aslice low# (inc v1) v2))))
                            (aset zone-confirmation-candle# v1 (bear-expand-after v1 8 data))
                            (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                            (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                            (aset zone-type# v1 :swing-point)))
                        (recur (inc j))))))
             0 nil)
           (recur (inc v1))))
       (assoc data :zone-sp2# zone#
                   :zone-sp2-open# zone-open#
                   :zone-sp2-close# zone-close#
                   :zone-sp2-contraction-count# zone-contraction-count#
                   :zone-sp2-contraction-size# zone-contraction-size#
                   :zone-sp2-confirmation-candle# zone-confirmation-candle#
                   :zone-sp2-size# zone-size#
                   :zone-sp2-filled-by# zone-filled-by#
                   :zone-sp2-type# zone-type#)))
    ([{:keys [^longs zone-sp2# ^doubles zone-sp2-open# ^doubles zone-sp2-close#
              ^longs zone-sp2-contraction-count# ^doubles zone-sp2-contraction-size#
              ^longs zone-sp2-confirmation-candle#
              ^doubles zone-sp2-size# ^longs zone-sp2-filled-by#
              ^objects zone-sp2-type#] :as data} ^long contraction-size ^long i]
     (if zone-sp2#
       {:zone-sp2#                     (aget zone-sp2# i)
        :zone-sp2-open#                (aget zone-sp2-open# i)
        :zone-sp2-close#               (aget zone-sp2-close# i)
        :zone-sp2-contraction-count#   (aget zone-sp2-contraction-count# i)
        :zone-sp2-contraction-size#    (aget zone-sp2-contraction-size# i)
        :zone-sp2-confirmation-candle# (aget zone-sp2-confirmation-candle# i)
        :zone-sp2-size#                (aget zone-sp2-size# i)
        :zone-sp2-filled-by#           (aget zone-sp2-filled-by# i)
        :zone-sp2-type#                (aget zone-sp2-type# i)}
       (recur (swing-point-2 data contraction-size) contraction-size i))))
  (swing-point-3
    (^Quotes [^Quotes {:keys [^objects time# ^doubles open# ^doubles high# ^doubles low#
                              ^doubles close# ^longs candle-direction# ^doubles candle-body-size#
                              ^doubles candle-upper-shadow# ^doubles candle-bottom-shadow#
                              ^longs candle-filled-by# ^longs candle-high-broken-by#
                              ^longs candle-low-broken-by# ^longs wrb-hg-body#
                              ^longs prior-unfilled-bull-wrb-hg# ^longs prior-unfilled-bear-wrb-hg#]
                       :as   data}]
     (let [array-size (alength time#)
           zone# (long-array array-size)
           zone-open# (double-array array-size)
           zone-close# (double-array array-size)
           zone-confirmation-candle# (long-array array-size)
           zone-size# (double-array array-size)
           zone-filled-by# (long-array array-size)
           zone-type# (object-array array-size)]
       (loop [i 4]
         (when (< i (- array-size 3))
           (cond
             (and (> (aget candle-bottom-shadow# i)
                     (+ (aget candle-body-size# i)
                        (aget candle-upper-shadow# i)))
                  (> (aget candle-body-size# i)
                     (aget candle-upper-shadow# i))
                  (> (aget candle-bottom-shadow# i)
                     (amax (aslice candle-bottom-shadow# (+ i 1) (+ i 4))))
                  (> (aget candle-bottom-shadow# i)
                     (amax (aslice candle-body-size# (+ i 1) (+ i 4))))
                  (< (aget low# i)
                     (amin (aslice low# (+ i 1) (+ i 8)))))
             (let [ver1 (loop [j 1]
                          (when (and (< (+ j 3) 16) (<= (+ j 3) i))
                            (if (and (== 1 (aget candle-direction# (- i j)))
                                     (== 1 (aget candle-direction# (- i j 1)))
                                     (== 1 (aget candle-direction# (- i j 2)))
                                     (or (== 1 (aget wrb-hg-body# (- i j)))
                                         (== 1 (aget wrb-hg-body# (- i j 1)))
                                         (== 1 (aget wrb-hg-body# (- i j 2)))))
                              (- i j 2)
                              (recur (inc j)))))
                   ver2 (let [prior-hg (aget prior-unfilled-bear-wrb-hg# i)]
                          (when (and (pos? prior-hg)
                                     (> (aget candle-filled-by# prior-hg)
                                        (aget candle-low-broken-by# i))
                                     (>= (aget candle-filled-by# prior-hg) (FastMath/max 0 (- i 8))))
                            (aget candle-filled-by# prior-hg)))]
               (when ver1
                 (do
                   (aset zone# i 1)
                   (aset zone-open# i (aget low# i))
                   (aset zone-close# i (FastMath/min (aget open# i) (aget close# i)))
                   (aset zone-confirmation-candle# i ^long ver1)
                   (aset zone-size# i (- (FastMath/min (aget open# i) (aget close# i)) (aget low# i)))
                   (aset zone-filled-by# i (aget candle-low-broken-by# i)))))

             (and (> (aget candle-upper-shadow# i)
                     (+ (aget candle-body-size# i)
                        (aget candle-bottom-shadow# i)))
                  (> (aget candle-body-size# i)
                     (aget candle-bottom-shadow# i))
                  (> (aget candle-upper-shadow# i)
                     (amax (aslice candle-upper-shadow# (+ i 1) (+ i 4))))
                  (> (aget candle-upper-shadow# i)
                     (amax (aslice candle-body-size# (+ i 1) (+ i 4))))
                  (> (aget high# i)
                     (amax (aslice high# (+ i 1) (+ i 8)))))
             (let [ver1 (loop [j 1]
                          (when (and (< (+ j 3) 16) (<= (+ j 3) i))
                            (if (and (== -1 (aget candle-direction# (- i j)))
                                     (== -1 (aget candle-direction# (- i j 1)))
                                     (== -1 (aget candle-direction# (- i j 2)))
                                     (or (== -1 (aget wrb-hg-body# (- i j)))
                                         (== -1 (aget wrb-hg-body# (- i j 1)))
                                         (== -1 (aget wrb-hg-body# (- i j 2)))))
                              (- i j 2)
                              (recur (inc j)))))
                   ver2 (let [prior-hg (aget prior-unfilled-bull-wrb-hg# i)]
                          (when (and (pos? prior-hg)
                                     (> (aget candle-filled-by# prior-hg)
                                        (aget candle-high-broken-by# i))
                                     (>= (aget candle-filled-by# prior-hg) (FastMath/max 0 (- i 8))))
                            (aget candle-filled-by# prior-hg)))]
               (when ver1
                 (do
                   (aset zone# i -1)
                   (aset zone-open# i (aget high# i))
                   (aset zone-close# i (FastMath/max (aget open# i) (aget close# i)))
                   (aset zone-confirmation-candle# i ^long ver1)
                   (aset zone-size# i (- (aget high# i) (FastMath/max (aget open# i) (aget close# i))))
                   (aset zone-filled-by# i (aget candle-high-broken-by# i))
                   (aset zone-type# i :swing-point)))))
           (recur (inc i))))
       (assoc data :zone-sp3# zone#
                   :zone-sp3-open# zone-open#
                   :zone-sp3-close# zone-close#
                   :zone-sp3-confirmation-candle# zone-confirmation-candle#
                   :zone-sp3-size# zone-size#
                   :zone-sp3-filled-by# zone-filled-by#
                   :zone-sp3-type# zone-type#)))
    ([{:keys [^longs zone-sp3# ^doubles zone-sp3-open# ^doubles zone-sp3-close#
              ^longs zone-sp3-confirmation-candle# ^doubles zone-sp3-size#
              ^longs zone-sp3-filled-by# ^objects zone-sp3-type#] :as data}
      ^long contraction-size ^long i]
     (if zone-sp3#
       {:zone-sp3#                     (aget zone-sp3# i)
        :zone-sp3-open#                (aget zone-sp3-open# i)
        :zone-sp3-close#               (aget zone-sp3-close# i)
        :zone-sp3-confirmation-candle# (aget zone-sp3-confirmation-candle# i)
        :zone-sp3-size#                (aget zone-sp3-size# i)
        :zone-sp3-filled-by#           (aget zone-sp3-filled-by# i)
        :zone-sp3-type#                (aget zone-sp3-type# i)}
       (recur (swing-point-3 data contraction-size) contraction-size i))))
  (strong-continuation-1
    (^Quotes [^Quotes {:keys [^objects time# ^doubles open# ^doubles high# ^doubles low#
                              ^doubles close# ^longs candle-broken-range# ^longs candle-filled-by#
                              ^longs wrb-hg-body# ^longs candle-fractal-break#
                              ^longs candle-fractal-break-nr#]
                       :as   data} ^long contraction-size]
     (let [array-size (alength time#)
           zone# (long-array array-size)
           zone-open# (double-array array-size)
           zone-close# (double-array array-size)
           zone-contraction-count# (long-array array-size)
           zone-contraction-size# (double-array array-size)
           zone-confirmation-candle# (long-array array-size)
           zone-size# (double-array array-size)
           zone-filled-by# (long-array array-size)
           zone-type# (object-array array-size)]
       (loop [v1 4]
         (when (< v1 (- array-size 3))
           (case (aget wrb-hg-body# v1)
             1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                 (loop [j 4]
                   (when (< j loop-limit)
                     (let [v2 (+ v1 j)]
                       (if (and
                             ;V1 must be bull
                             (== 1 (aget wrb-hg-body# v1))
                             ;V2 must be wrb
                             (== 1 (aget wrb-hg-body# v2))
                             ;V2 shuld be unfilled until v1
                             (<= (aget candle-filled-by# v2) v1)
                             ;V2 shuld be breakout interval
                             (>= (aget candle-broken-range# v2) 3)
                             ;Contraction volatility must share min 1 pip with V2
                             (contraction-share? v1 v2 data)
                             ;V1 must breakout volatility contraction and
                             ;Body of V1/V2 must be greater than volatility
                             ;contraction
                             (contraction-break-bull? v1 v2 data)
                             ;V1 or V2 must be bull reaction break
                             (== 1 (aget candle-fractal-break# v1))
                             (<= 16 (- (aget candle-fractal-break-nr# v1) v1) 64)
                             (bull-expand-after v1 8 data))
                         (do
                           (aset zone# v1 (aget wrb-hg-body# v1))
                           (aset zone-open# v1 (aget open# v1))
                           (aset zone-close# v1 (aget close# v1))
                           (aset zone-contraction-count# v1 j)
                           (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                              (amin (aslice low# (inc v1) v2))))
                           (aset zone-confirmation-candle# v1 ^long (bull-expand-after v1 8 data))
                           (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                           (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                           (aset zone-type# v1 :strong-continuation))
                         (recur (inc j)))))))
             -1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                  (loop [j 4]
                    (when (< j loop-limit)
                      (let [v2 (+ v1 j)]
                        (if (and
                              ;V1 must be bear
                              (== -1 (aget wrb-hg-body# v1))
                              ;V2 must be wrb
                              (== -1 (aget wrb-hg-body# v2))
                              ;V2 shuld be unfilled until v1
                              (<= (aget candle-filled-by# v2) v1)
                              ;V2 shuld be breakout interval
                              (>= (aget candle-broken-range# v2) 3)
                              ;Contraction volatility must share min 1 pip with V2
                              (contraction-share? v1 v2 data)
                              ;V1 must breakout volatility contraction and
                              ;Body of V1/V2 must be greater than volatility
                              ;contraction
                              (contraction-break-bear? v1 v2 data)
                              ;V1 or V2 must be bear reaction break
                              (== -1 (aget candle-fractal-break# v1))
                              (<= 16 (- (aget candle-fractal-break-nr# v1) v1) 64)
                              (bear-expand-after v1 8 data))
                          (do
                            (aset zone# v1 (aget wrb-hg-body# v1))
                            (aset zone-open# v1 (aget open# v1))
                            (aset zone-close# v1 (aget close# v1))
                            (aset zone-contraction-count# v1 j)
                            (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                               (amin (aslice low# (inc v1) v2))))
                            (aset zone-confirmation-candle# v1 ^long (bear-expand-after v1 8 data))
                            (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                            (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                            (aset zone-type# v1 :strong-continuation))
                          (recur (inc j)))))))
             0 nil)
           (recur (inc v1))))
       (assoc data :zone-sc1# zone#
                   :zone-sc1-open# zone-open#
                   :zone-sc1-close# zone-close#
                   :zone-sc1-contraction-count# zone-contraction-count#
                   :zone-sc1-contraction-size# zone-contraction-size#
                   :zone-sc1-confirmation-candle# zone-confirmation-candle#
                   :zone-sc1-size# zone-size#
                   :zone-sc1-filled-by# zone-filled-by#
                   :zone-sc1-type# zone-type#)))
    ([{:keys [^longs zone-sc1# ^doubles zone-sc1-open# ^doubles zone-sc1-close#
              ^longs zone-sc1-contraction-count# ^doubles zone-sc1-contraction-size#
              ^longs zone-sc1-confirmation-candle#
              ^doubles zone-sc1-size# ^longs zone-sc1-filled-by#
              ^longs zone-sc1-zero-line# ^objects zone-sc1-type#] :as data} ^long contraction-size ^long i]
     (if zone-sc1#
       {:zone-sc1#                     (aget zone-sc1# i)
        :zone-sc1-open#                (aget zone-sc1-open# i)
        :zone-sc1-close#               (aget zone-sc1-close# i)
        :zone-sc1-contraction-count#   (aget zone-sc1-contraction-count# i)
        :zone-sc1-contraction-size#    (aget zone-sc1-contraction-size# i)
        :zone-sc1-confirmation-candle# (aget zone-sc1-confirmation-candle# i)
        :zone-sc1-size#                (aget zone-sc1-size# i)
        :zone-sc1-filled-by#           (aget zone-sc1-filled-by# i)
        :zone-sc1-zero-line#           (aget zone-sc1-zero-line# i)
        :zone-sc1-type#                (aget zone-sc1-type# i)}
       (recur (strong-continuation-1 data contraction-size) contraction-size i))))
  (strong-continuation-2
    (^Quotes [^Quotes {:keys [^objects time# ^doubles open# ^doubles high# ^doubles low#
                              ^doubles close# ^longs candle-direction# ^longs candle-broken-range#
                              ^longs candle-filled-by# ^longs wrb-hg-body#]
                       :as   data} ^long contraction-size]
     (let [array-size (alength time#)
           zone# (long-array array-size)
           zone-open# (double-array array-size)
           zone-close# (double-array array-size)
           zone-contraction-count# (long-array array-size)
           zone-contraction-size# (double-array array-size)
           zone-confirmation-candle# (long-array array-size)
           zone-size# (double-array array-size)
           zone-filled-by# (long-array array-size)
           zone-type# (object-array array-size)]
       (loop [v1 4]
         (when (< v1 (- array-size 3))
           (case (aget wrb-hg-body# v1)
             1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                 (loop [i 4]
                   (when (< i loop-limit)
                     (let [v2 (+ v1 i)]
                       (if (and
                             ;V1 must be bull
                             (== 1 (aget wrb-hg-body# v1))
                             ;V2 must be wrb
                             (== 1 (aget wrb-hg-body# v2))
                             ;V2 shuld be volatility spike
                             (>= (aget candle-broken-range# v2) 3)
                             ;Must be volatility expand after v1
                             (bull-expand-after v1 8 data)
                             ;(and (== 1 (aget candle-direction# (dec v1)))
                             ;     (== 1 (aget candle-direction# (- v1 2)))
                             ;     (> (FastMath/min (aget close# (- v1 2)) (aget close# (dec v1)))
                             ;        (aget close# v1)))
                             ;(weak-expand-bull-after v1 data)
                             ;Must be volatility expand before v2
                             (bull-expand-before v2 8 data)
                             ;Volatility contraction can't retrace 3 interval befor v2
                             ;(< (amin (aslice low# v2 (+ v2 4)))
                             ;   (amin (aslice low# (inc v1) v2)))
                             ;V2 shuld be unfilled until v1
                             (<= (aget candle-filled-by# v2) v1)
                             ;Contraction volatility must share min 1 pip with V2
                             (contraction-share? v1 v2 data)
                             ;V1 must breakout volatility contraction and
                             ;Body of V1/V2 must be greater than volatility
                             ;contraction
                             (contraction-break-bull? v1 v2 data))
                         (do
                           (aset zone# v1 (aget wrb-hg-body# v1))
                           (aset zone-open# v1 (aget open# v1))
                           (aset zone-close# v1 (aget close# v1))
                           (aset zone-contraction-count# v1 i)
                           (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                              (amin (aslice low# (inc v1) v2))))
                           (aset zone-confirmation-candle# v1 ^long (bull-expand-after v1 8 data))
                           (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                           (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                           (aset zone-type# v1 :strong-continuation))
                         (recur (inc i)))))))
             -1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                  (loop [i 4]
                    (when (< i loop-limit)
                      (let [v2 (+ v1 i)]
                        (if (and
                              ;V1 must be bear
                              (== -1 (aget wrb-hg-body# v1))
                              ;V2 must be wrb-hg
                              (== -1 (aget wrb-hg-body# v2))
                              ;V2 shuld be volatility spike
                              (>= (aget candle-broken-range# v2) 3)
                              ;Must be volatility expand after v1
                              (bear-expand-after v1 8 data)
                              ;(and (== -1 (aget candle-direction# (dec v1)))
                              ;     (== -1 (aget candle-direction# (- v1 2)))
                              ;     (< (FastMath/min (aget close# (- v1 2)) (aget close# (dec v1)))
                              ;        (aget close# v1)))
                              ;(weak-expand-bear-after v1 data)
                              ;Must be volatility expand before v2
                              (bear-expand-before v2 8 data)
                              ;Volatility contraction can't retrace 3 interval befor v2
                              ;(> (amax (aslice high# v2 (+ v2 4)))
                              ;   (amax (aslice high# (inc v1) v2)))
                              ;V2 shuld be unfilled until v1
                              (<= (aget candle-filled-by# v2) v1)
                              ;Contraction volatility must share min 1 pip with V2
                              (contraction-share? v1 v2 data)
                              ;V1 must breakout volatility contraction and
                              ;Body of V1/V2 must be greater than volatility
                              ;contraction
                              (contraction-break-bear? v1 v2 data))
                          (do
                            (aset zone# v1 (aget wrb-hg-body# v1))
                            (aset zone-open# v1 (aget open# v1))
                            (aset zone-close# v1 (aget close# v1))
                            (aset zone-contraction-count# v1 i)
                            (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                               (amin (aslice low# (inc v1) v2))))
                            (aset zone-confirmation-candle# v1 ^long (bear-expand-after v1 8 data))
                            (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                            (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                            (aset zone-type# v1 :strong-continuation))
                          (recur (inc i)))))))
             0 nil)
           (recur (inc v1))))
       (assoc data :zone-sc2# zone#
                   :zone-sc2-open# zone-open#
                   :zone-sc2-close# zone-close#
                   :zone-sc2-contraction-count# zone-contraction-count#
                   :zone-sc2-contraction-size# zone-contraction-size#
                   :zone-sc2-confirmation-candle# zone-confirmation-candle#
                   :zone-sc2-size# zone-size#
                   :zone-sc2-filled-by# zone-filled-by#
                   :zone-sc2-type# zone-type#)))
    ([{:keys [^longs zone-sc2# ^doubles zone-sc2-open# ^doubles zone-sc2-close#
              ^longs zone-sc2-contraction-count# ^doubles zone-sc2-contraction-size#
              ^longs zone-sc2-confirmation-candle#
              ^doubles zone-sc2-size# ^longs zone-sc2-filled-by#
              ^objects zone-sc2-type#] :as data} ^long contraction-size ^long i]
     (if zone-sc2#
       {:zone-sc2#                     (aget zone-sc2# i)
        :zone-sc2-open#                (aget zone-sc2-open# i)
        :zone-sc2-close#               (aget zone-sc2-close# i)
        :zone-sc2-contraction-count#   (aget zone-sc2-contraction-count# i)
        :zone-sc2-contraction-size#    (aget zone-sc2-contraction-size# i)
        :zone-sc2-confirmation-candle# (aget zone-sc2-confirmation-candle# i)
        :zone-sc2-size#                (aget zone-sc2-size# i)
        :zone-sc2-filled-by#           (aget zone-sc2-filled-by# i)
        :zone-sc2-type#                (aget zone-sc2-type# i)}
       (recur (strong-continuation-2 data contraction-size) contraction-size i))))
  (strong-continuation-3
    (^Quotes [^Quotes {:keys [^objects time# ^doubles open# ^doubles high# ^doubles low#
                              ^doubles close# ^longs candle-direction#
                              ^doubles candle-body-midpoint# ^longs candle-broken-range#
                              ^longs candle-filled-by# ^longs wrb-hg-body#]
                       :as   data} ^long contraction-size]
     (let [array-size (alength time#)
           zone# (long-array array-size)
           zone-open# (double-array array-size)
           zone-close# (double-array array-size)
           zone-contraction-count# (long-array array-size)
           zone-contraction-size# (double-array array-size)
           zone-confirmation-candle# (long-array array-size)
           zone-size# (double-array array-size)
           zone-filled-by# (long-array array-size)
           zone-type# (object-array array-size)]
       (loop [v1 4]
         (when (< v1 (- array-size 3))
           (case (aget wrb-hg-body# v1)
             1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                 (loop [i 4]
                   (when (< i loop-limit)
                     (let [v2 (+ v1 i)]
                       (when (and
                               ;V1 must be bull
                               ;V2 must be wrb hg
                               (== 1 (aget wrb-hg-body# v2))
                               ;V2 shuld be breakout interval
                               (>= (aget candle-broken-range# v2) 3)
                               ;V1 must have the bodies of the prior contracting volatility
                               ;< body mid-point of j or i must have the bodies of the prior
                               ;contracting volatility < body mid-point of i
                               (or (> (aget candle-body-midpoint# v2)
                                      (amax (aslice close# (inc v2) (+ v2 4))))
                                   (> (aget candle-body-midpoint# v1)
                                      (amax (aslice close# (inc v1) (+ v1 4)))))
                               ;Contraction volatility must share min 1 pip with V2
                               (contraction-share? v1 v2 data)
                               ;V1 must breakout volatility contraction and
                               ;Body of V1/V2 must be greater than volatility
                               ;contraction
                               (contraction-break-bull? v1 v2 data)
                               ;Must be volatility expand after v1
                               (bull-expand-after v1 8 data))
                         (aset zone# v1 (aget candle-direction# v1))
                         (aset zone-open# v1 (aget open# v1))
                         (aset zone-close# v1 (aget close# v1))
                         (aset zone-contraction-count# v1 i)
                         (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                            (amin (aslice low# (inc v1) v2))))
                         (aset zone-confirmation-candle# v1 ^long (bull-expand-after v1 8 data))
                         (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                         (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                         (aset zone-type# v1 :strong-continuation)))
                     (recur (inc i)))))
             -1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                  (loop [i 4]
                    (when (< i loop-limit)
                      (let [v2 (+ v1 i)]
                        (when (and
                                ;V1 must be bear
                                ;V2 must be wrb-hg
                                (== -1 (aget wrb-hg-body# v2))
                                ;V2 shuld be breakout interval
                                (>= (aget candle-broken-range# v2) 3)
                                ;V1 must have the bodies of the prior contracting volatility
                                ;< body mid-point of j or i must have the bodies of the prior
                                ;contracting volatility < body mid-point of i
                                (or (< (aget candle-body-midpoint# v2)
                                       (amin (aslice close# (inc v2) (+ v2 4))))
                                    (< (aget candle-body-midpoint# v1)
                                       (amin (aslice close# (inc v1) (+ v1 4)))))
                                ;Contraction volatility must share min 1 pip with V2
                                (contraction-share? v1 v2 data)
                                ;V1 must breakout volatility contraction and
                                ;Body of V1/V2 must be greater than volatility
                                ;contraction
                                (contraction-break-bear? v1 v2 data)
                                (bear-expand-after v1 8 data))
                          (aset zone# v1 (aget candle-direction# v1))
                          (aset zone-open# v1 (aget open# v1))
                          (aset zone-close# v1 (aget close# v1))
                          (aset zone-contraction-count# v1 i)
                          (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                             (amin (aslice low# (inc v1) v2))))
                          (aset zone-confirmation-candle# v1 ^long (bear-expand-after v1 8 data))
                          (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                          (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                          (aset zone-type# v1 :strong-continuation)))
                      (recur (inc i)))))
             0 nil)
           (recur (inc v1))))
       (assoc data :zone-sc3# zone#
                   :zone-sc3-open# zone-open#
                   :zone-sc3-close# zone-close#
                   :zone-sc3-contraction-count# zone-contraction-count#
                   :zone-sc3-confirmation-candle# zone-confirmation-candle#
                   :zone-sc3-size# zone-size#
                   :zone-sc3-filled-by# zone-filled-by#
                   :zone-sc3-type# zone-type#)))
    ([{:keys [^longs zone-sc3# ^doubles zone-sc3-open# ^doubles zone-sc3-close#
              ^longs zone-sc3-contraction-count# ^doubles zone-sc3-contraction-size#
              ^longs zone-sc3-confirmation-candle#
              ^doubles zone-sc3-size# ^longs zone-sc3-filled-by#
              ^objects zone-sc3-zone-type#] :as data} ^long contraction-size ^long i]
     (if zone-sc3#
       {:zone-sc3#                     (aget zone-sc3# i)
        :zone-sc3-open#                (aget zone-sc3-open# i)
        :zone-sc3-close#               (aget zone-sc3-close# i)
        :zone-sc3-contraction-count#   (aget zone-sc3-contraction-count# i)
        :zone-sc3-contraction-size#    (aget zone-sc3-contraction-size# i)
        :zone-sc3-confirmation-candle# (aget zone-sc3-confirmation-candle# i)
        :zone-sc3-size#                (aget zone-sc3-size# i)
        :zone-sc3-filled-by#           (aget zone-sc3-filled-by# i)
        :zone-sc3-zone-type#           (aget zone-sc3-zone-type# i)}
       (recur (strong-continuation-3 data contraction-size) contraction-size i))))
  (strong-continuation-4
    (^Quotes [^Quotes {:keys [^objects time# ^doubles open# ^doubles high# ^doubles low# ^doubles close#
                              ^longs candle-direction# ^doubles candle-body-midpoint#
                              ^longs candle-broken-range# ^longs candle-filled-by# ^longs wrb-hg-body#]
                       :as   data} ^long contraction-size]
     (let [array-size (alength time#)
           zone# (long-array array-size)
           zone-open# (double-array array-size)
           zone-close# (double-array array-size)
           zone-contraction-count# (long-array array-size)
           zone-contraction-size# (double-array array-size)
           zone-confirmation-candle# (long-array array-size)
           zone-size# (double-array array-size)
           zone-filled-by# (long-array array-size)
           zone-type# (object-array array-size)]
       (loop [v1 4]
         (when (< v1 (- array-size 3))
           (condp == (aget wrb-hg-body# v1)
             1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                 (loop [i 4]
                   (when (< i loop-limit)
                     (let [v2 (+ v1 i)]
                       (if (and
                             ;V1 must be bull
                             (== 1 (aget wrb-hg-body# v1))
                             ; v1 close# > v2 close#
                             (> (aget close# v1) (aget close# v2))
                             ;V2 must be wrb
                             (== 1 (aget wrb-hg-body# v2))
                             ;V2 shuld be breakout interval
                             (>= (aget candle-broken-range# v2) 3)
                             ;V1 must have the bodies of the prior contracting volatility
                             ;< body mid-point of j or i must have the bodies of the prior
                             ;contracting volatility < body mid-point of i
                             (< (aget candle-body-midpoint# v2)
                                (amin (aslice low# (+ v1 1) v2)))
                             ;Contraction volatility must share min 1 pip with V2
                             (contraction-share? v1 v2 data)
                             ;V1 must breakout volatility contraction and
                             ;Body of V1/V2 must be greater than volatility
                             ;contraction
                             (contraction-break-bull? v1 v2 data)
                             (bull-expand-after v1 8 data))
                         (do
                           (aset zone# v1 (aget candle-direction# v1))
                           (aset zone-open# v1 (aget open# v1))
                           (aset zone-close# v1 (aget close# v1))
                           (aset zone-contraction-count# v1 i)
                           (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                              (amin (aslice low# (inc v1) v2))))
                           (aset zone-confirmation-candle# v1 ^long (bull-expand-after v1 8 data))
                           (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                           (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                           (aset zone-type# v1 :strong-continuation))
                         (recur (inc i)))))))
             -1 (let [loop-limit (FastMath/min contraction-size ^long (- array-size v1))]
                  (loop [i 4]
                    (when (< i loop-limit)
                      (let [v2 (+ v1 i)]
                        (if (and
                              ;V1 must be bear
                              (== -1 (aget wrb-hg-body# v1))
                              ; v1 close# > v2 close#
                              (< (aget close# v1) (aget close# v2))
                              ;V2 must be wrb-hg
                              (== -1 (aget wrb-hg-body# v2))
                              ;V2 shuld be breakout interval
                              (>= (aget candle-broken-range# v2) 3)
                              ;V1 must have the bodies of the prior contracting volatility
                              ;< body mid-point of j or i must have the bodies of the prior
                              ;contracting volatility < body mid-point of i
                              (> (aget candle-body-midpoint# v2)
                                 (amax (aslice high# (+ v1 1) v2)))
                              ;Contraction volatility must share min 1 pip with V2
                              (contraction-share? v1 v2 data)
                              ;V1 must breakout volatility contraction and
                              ;Body of V1/V2 must be greater than volatility
                              ;contraction
                              (contraction-break-bear? v1 v2 data)
                              (bear-expand-after v1 8 data))
                          (do
                            (aset zone# v1 (aget candle-direction# v1))
                            (aset zone-open# v1 (aget open# v1))
                            (aset zone-close# v1 (aget close# v1))
                            (aset zone-contraction-count# v1 i)
                            (aset zone-contraction-size# v1 (- (amax (aslice high# (inc v1) v2))
                                                               (amin (aslice low# (inc v1) v2))))
                            (aset zone-confirmation-candle# v1 ^long (bear-expand-after v1 8 data))
                            (aset zone-size# v1 (FastMath/abs (- (aget open# v1) (aget close# v1))))
                            (aset zone-filled-by# v1 (aget candle-filled-by# v1))
                            (aset zone-type# v1 :strong-continuation))
                          (recur (inc i)))))))
             0 nil)
           (recur (inc v1))))
       (assoc data :zone-sc4# zone#
                   :zone-sc4-open# zone-open#
                   :zone-sc4-close# zone-close#
                   :zone-sc4-contraction-count# zone-contraction-count#
                   :zone-sc4-contraction-size# zone-contraction-size#
                   :zone-sc4-confirmation-candle# zone-confirmation-candle#
                   :zone-sc4-size# zone-size#
                   :zone-sc4-filled-by# zone-filled-by#
                   :zone-sc4-type# zone-type#)))
    ([{:keys [^longs zone-sc4# ^doubles zone-sc4-open# ^doubles zone-sc4-close#
              ^longs zone-sc4-contraction-count# ^doubles zone-sc4-contraction-size#
              ^longs zone-sc4-confirmation-candle#
              ^doubles zone-sc4-size# ^longs zone-sc4-filled-by#
              ^objects zone-sc4-zone-type#] :as data} ^long contraction-size ^long i]
     (if zone-sc4#
       {:zone-sc4#                     (aget zone-sc4# i)
        :zone-sc4-open#                (aget zone-sc4-open# i)
        :zone-sc4-close#               (aget zone-sc4-close# i)
        :zone-sc4-contraction-count#   (aget zone-sc4-contraction-count# i)
        :zone-sc4-contraction-size#    (aget zone-sc4-contraction-size# i)
        :zone-sc4-confirmation-candle# (aget zone-sc4-confirmation-candle# i)
        :zone-sc4-size#                (aget zone-sc4-size# i)
        :zone-sc4-filled-by#           (aget zone-sc4-filled-by# i)
        :zone-sc4-zone-type#           (aget zone-sc4-zone-type# i)}
       (recur (strong-continuation-4 data contraction-size) contraction-size i))))


  ;(defn strong-continuation-5
  ;  ^Quotes [^Quotes data & {:keys [contraction-size]
  ;                           :or   {contraction-size 32}}]
  ;  (let [array-size (alength time#)
  ;        ^doubles open# (:open# data)
  ;        ^doubles high# (:high# data)
  ;        ^doubles low# (:low# data)
  ;        ^doubles close# (:close# data)
  ;        ^longs direction# (:candle-direction# data)
  ;        ^longs fractal# (:candle-fractal data)
  ;        ^longs hg# (:wrb-hg-body# data)
  ;        ^longs zone# (or (:zone# data)
  ;                         (boolean-array array-size))
  ;        ^longs zone-sc5# (boolean-array array-size)
  ;        ^doubles zone-open# (or (:zone-open# data)
  ;                                (double-array array-size))
  ;        ^doubles zone-close# (or (:zone-close# data)
  ;                                 (double-array array-size))
  ;        ^longs zone-contraction-count# (or (:zone-contraction-count# data)
  ;                                     (long-array array-size))
  ;        ^doubles zone-size# (or (:zone-size# data)
  ;                                (double-array array-size))]
  ;    (loop [v 4]
  ;      (when (< v (- array-size 10))
  ;        (when (not= (aget hg# v))
  ;          (let [loop-limit (min contraction-size v)]
  ;            (loop [i 5]
  ;              (when (< i loop-limit)
  ;                (let [r (- v i)]
  ;                  (if
  ;                    (or
  ;                      (and
  ;                        ;v must be bull
  ;                        (== 1 (aget direction# v))
  ;                        ;wrb-hg must be unfiled
  ;                        (> (aget low# r)
  ;                           (aget open# v))
  ;                        (<= (aget low# r)
  ;                            (amin (aslice low#
  ;                                          (inc r) v)))
  ;                        ;(< (aget high# (inc v))
  ;                        ;   (aget low# r)
  ;                        ;   (aget low# (dec v)))
  ;                        (== 1 (aget fractal# r)))
  ;
  ;                      (and
  ;                        ;v must be bear
  ;                        (== -1 (aget direction# v))
  ;                        ;wrb-hg must be unfiled
  ;                        (< (aget high# r)
  ;                           (aget open# v))
  ;                        (>= (aget high# r)
  ;                            (amax (aslice high#
  ;                                          (inc r) v)))
  ;                        ;(> (aget low# (inc v))
  ;                        ;   (aget high# r)
  ;                        ;   (aget high# (dec v)))
  ;                        (== -1 (aget fractal# r))))
  ;
  ;                    (aset zone# v (aget direction# v))
  ;                    (aset zone-sc5# v (aget direction# v))
  ;                    (aset zone-open# v
  ;                          (aget open# v))
  ;                    (aset zone-close# v
  ;                          (aget close# v))
  ;                    (aset zone-contraction-count# v i)
  ;                    (aset zone-size# v (FastMath/abs (- (aget open# v)
  ;                                                    (aget close# v)))))
  ;                  (recur (inc i)))))))
  ;        (recur (inc v))))
  ;    (-> data
  ;        (assoc :zone# zone#)
  ;        (assoc :zone-sc5? zone-sc5#)
  ;        (assoc :zone-open# zone-open#)
  ;        (assoc :zone-close# zone-close#)
  ;        (assoc :zone-contraction-count# zone-contraction-count#)
  ;        (assoc :zone-size# zone-size#)
  ; (assoc :zone-filled-by# zone-filled-by#)
  ; (assoc :zone-zero-line zone-zero-line))))


  ;(defn zone-prior
  ;  (^Quotes [^Quotes data ^Keyword zone-type]
  ;    (let [array-size (alength time#)
  ;          loop-limit (dec array-size)
  ;          ^longs zone# (get data zone-type)
  ;          ^longs zone-filled-by# (get data (filled-by-keyword zone-type))
  ;          ^longs zone-prior-bull-nr# (long-array array-size)
  ;          ^longs zone-prior-bear-nr# (long-array array-size)]
  ;      (loop [i 0]
  ;        (when (< i loop-limit)
  ;          (loop [j 1]
  ;            (if (< (+ i j) array-size)
  ;              (if (and (== 1 (aget zone# (+ i j)))
  ;                       (<= (aget zone-filled-by# (+ i j)) i))
  ;                (aset zone-prior-bull-nr# i (+ i j))
  ;                (recur (inc j)))
  ;              (aset zone-prior-bull-nr# i -1)))
  ;          (loop [j 1]
  ;            (if (< (+ i j) array-size)
  ;              (if (and (== -1 (aget zone# (+ i j)))
  ;                       (<= (aget zone-filled-by# (+ i j)) i))
  ;                (aset zone-prior-bear-nr# i (+ i j))
  ;                (recur (inc j)))
  ;              (aset zone-prior-bear-nr# i -1)))
  ;          (recur (inc i))))
  ;      (assoc data
  ;        (keyword (generate-keyword "" zone-type "-prior-bull-nr")) zone-prior-bull-nr#
  ;        (keyword (generate-keyword "" zone-type "-prior-bear-nr")) zone-prior-bear-nr#))))
  ;(defn zone-next
  ;  ^Quotes [^Quotes data ^Keyword zone-type]
  ;  (let [array-size (alength time#)
  ;        loop-limit (dec array-size)
  ;        ^longs zone# (get data zone-type)
  ;        ^longs zone-next-bull-nr# (long-array array-size)
  ;        ^longs zone-next-bear-nr# (long-array array-size)]
  ;    (loop [i 0]
  ;      (when (< i loop-limit)
  ;        (loop [j 1]
  ;          (if (< j i)
  ;            (if (== 1 (aget zone# (- i j)))
  ;              (aset zone-next-bull-nr# i (- i j))
  ;              (recur (inc j)))
  ;            (aset zone-next-bull-nr# i -1)))
  ;        (loop [j 1]
  ;          (if (< j i)
  ;            (if (== -1 (aget zone# (- i j)))
  ;              (aset zone-next-bear-nr# i (- i j))
  ;              (recur (inc j)))
  ;            (aset zone-next-bear-nr# i -1)))
  ;        (recur (inc i))))
  ;    (assoc data (generate-keyword "" zone-type "-next-bull-nr") zone-next-bull-nr#
  ;                (generate-keyword "" zone-type "-next-bull-nr") zone-next-bear-nr#)))
  )


(defn analyse
  ^Quotes [^Quotes data ^long contraction-size]
  (-> data
      (swing-point-1 contraction-size)
      ;(zone-prior :zone-sp1)
      ;(zone-next :zone-sp1)
      ;(zone-inside :zone-sp1)
      ;(zone-bounce-candle :zone-sp1)
      ;(zone-bounce-h2 :zone-sp1)

      (swing-point-2 contraction-size)
      ;(zone-prior :zone-sp2)
      ;(zone-next :zone-sp2)
      ;(zone-inside :zone-sp2)
      ;(zone-bounce-candle :zone-sp2)
      ;(zone-bounce-h2 :zone-sp2)

      (swing-point-3)
      ;(zone-prior :zone-sp3)
      ;(zone-next :zone-sp3)
      ;(zone-inside :zone-sp3)
      ;(zone-bounce-candle :zone-sp3)
      ;(zone-bounce-h2 :zone-sp3)

      (strong-continuation-1 contraction-size)
      ;(zone-prior :zone-sc1)
      ;(zone-next :zone-sc1)
      ;(zone-inside :zone-sc1)
      ;(zone-bounce-candle :zone-sc1)
      ;(zone-bounce-h2 :zone-sc1)

      (strong-continuation-2 contraction-size)
      ;(zone-prior :zone-sc2)
      ;(zone-next :zone-sc2)
      ;(zone-inside :zone-sc2)
      ;(zone-bounce-candle :zone-sc2)
      ;(zone-bounce-h2 :zone-sc2)

      (strong-continuation-3 contraction-size)
      ;(zone-prior :zone-sc3)
      ;(zone-next :zone-sc3)
      ;(zone-inside :zone-sc3)
      ;(zone-bounce-candle :zone-sc3)
      ;(zone-bounce-h2 :zone-sc3)

      (strong-continuation-4 contraction-size)
      ;(zone-prior :zone-sc4)
      ;(zone-next :zone-sc4)
      ;(zone-inside :zone-sc4)
      ;(zone-bounce-candle :zone-sc4)
      ;(zone-bounce-h2 :zone-sc4)
      ))




;(defn analyse-profile
;  [^Quotes data & {:keys [contraction-size]
;                   :or   {contraction-size 16}}]
;  (taoensso.timbre.profiling/p :swing-point-1 (swing-point-1 data contraction-size))
;  (taoensso.timbre.profiling/p :swing-point-2 (swing-point-2 data contraction-size))
;  (taoensso.timbre.profiling/p :swing-point-3 (swing-point-3 data))
;  (taoensso.timbre.profiling/p :prior-swing-point (prior-swing-point data))
;  (taoensso.timbre.profiling/p :next-swing-point (next-swing-point data))
;  (taoensso.timbre.profiling/p :strong-continuation-1 (strong-continuation-1 data contraction-size))
;  (taoensso.timbre.profiling/p :strong-continuation-2 (strong-continuation-2 data contraction-size))
;  (taoensso.timbre.profiling/p :strong-continuation-3 (strong-continuation-3 data contraction-size))
;  (taoensso.timbre.profiling/p :strong-continuation-4 (strong-continuation-4 data contraction-size))
;  (taoensso.timbre.profiling/p :prior-strong-continuation (prior-strong-continuation data))
;  (taoensso.timbre.profiling/p :next-strong-continuation (next-strong-continuation data))
;  (taoensso.timbre.profiling/p :prior-zone# (prior-zone# data))
;  (taoensso.timbre.profiling/p :next-zone# (next-zone# data))
;  (taoensso.timbre.profiling/p :inside-zone# (inside-zone# data))
;  )
