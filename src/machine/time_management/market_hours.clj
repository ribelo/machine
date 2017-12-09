(ns machine.time-management.market-hours
  (:require [schema.core :as s]
            [clj-time.core :as dt]
            [machine.time-management.core :refer :all])
  (:import [org.joda.time DateTime]))



(def london-hours {:open  8
                   :close 17})


(def frankfurt-hours {:open  7
                      :close 16})


(def new-york-hours {:open  13
                     :close 22})


(def chicago-hours {:open  14
                    :close 23})


(def tokyo-hours {:open  0
                  :close 9})


(def hong-kong-hours {:open  1
                      :close 10})


(def sydney-hours {:open  22
                   :close 7})


(def wellington-hours {:open  22
                       :close 6})


(s/defn london-market? :- s/Bool
  [data-time :- DateTime]
  (let [hour (dt/hour data-time)]
    (or (and (> (:close london-hours) (:open london-hours))
             (<= (:open london-hours) hour)
             (> (:close london-hours) hour))
        (and (< (:close london-hours) (:open london-hours))
             (or (<= (:open london-hours) hour 23)
                 (> (:close london-hours) hour 0))))))


(s/defn frankfurt-market? :- s/Bool
  [data-time :- DateTime]
  (let [hour (dt/hour data-time)]
    (or (and (> (:close frankfurt-hours) (:open frankfurt-hours))
             (<= (:open frankfurt-hours) hour)
             (> (:close frankfurt-hours) hour))
        (and (< (:close frankfurt-hours) (:open frankfurt-hours))
             (or (<= (:open frankfurt-hours) hour 23)
                 (> (:close frankfurt-hours) hour 0))))))


(s/defn new-york-market? :- s/Bool
  [data-time :- DateTime]
  (let [hour (dt/hour data-time)]
    (or (and (> (:close new-york-hours) (:open new-york-hours))
             (<= (:open new-york-hours) hour)
             (> (:close new-york-hours) hour))
        (and (< (:close new-york-hours) (:open new-york-hours))
             (or (<= (:open new-york-hours) hour 23)
                 (> (:close new-york-hours) hour 0))))))


(s/defn chicago-market? :- s/Bool
  [data-time :- DateTime]
  (let [hour (dt/hour data-time)]
    (or (and (> (:close chicago-hours) (:open chicago-hours))
             (<= (:open chicago-hours) hour)
             (> (:close chicago-hours) hour))
        (and (< (:close chicago-hours) (:open chicago-hours))
             (or (<= (:open chicago-hours) hour 23)
                 (> (:close chicago-hours) hour 0))))))


(s/defn tokyo-market? :- s/Bool
  [data-time :- DateTime]
  (let [hour (dt/hour data-time)]
    (or (and (> (:close tokyo-hours) (:open tokyo-hours))
             (<= (:open tokyo-hours) hour)
             (> (:close tokyo-hours) hour))
        (and (< (:close tokyo-hours) (:open tokyo-hours))
             (or (<= (:open tokyo-hours) hour 23)
                 (> (:close tokyo-hours) hour 0))))))


(s/defn hong-kong-market? :- s/Bool
  [data-time :- DateTime]
  (let [hour (dt/hour data-time)]
    (or (and (> (:close hong-kong-hours) (:open hong-kong-hours))
             (<= (:open hong-kong-hours) hour)
             (> (:close hong-kong-hours) hour))
        (and (< (:close hong-kong-hours) (:open hong-kong-hours))
             (or (<= (:open hong-kong-hours) hour 23)
                 (> (:close hong-kong-hours) hour 0))))))


(s/defn sydney-market? :- s/Bool
  [data-time :- DateTime]
  (let [hour (dt/hour data-time)]
    (or (and (> (:close sydney-hours) (:open sydney-hours))
             (<= (:open sydney-hours) hour)
             (> (:close sydney-hours) hour))
        (and (< (:close sydney-hours) (:open sydney-hours))
             (or (<= (:open sydney-hours) hour 23)
                 (> (:close sydney-hours) hour 0))))))


(s/defn wellington-market? :- s/Bool
  [data-time :- DateTime]
  (let [hour (dt/hour data-time)]
    (or (and (> (:close wellington-hours) (:open wellington-hours))
             (<= (:open wellington-hours) hour)
             (> (:close wellington-hours) hour))
        (and (< (:close wellington-hours) (:open wellington-hours))
             (or (<= (:open wellington-hours) hour 23)
                 (> (:close wellington-hours) hour 0))))))


(def ^:private trading-hours-map
  {:au200-aud  {:open 0 :close 24}
   :ch20-chf   {:open 6 :close 20}
   :de30-eur   {:open 6 :close 20}
   :eu50-eur   {:open 6 :close 20}
   :fr40-eur   {:open 6 :close 20}
   :hk33-hkd   {:open 16 :close 1}
   :jp225-usd  {:open 0 :close 24}
   :nl25-eur   {:open 6 :close 20}
   :sg30-sdg   {:open 0 :close 18}
   :spx500-usd {:open 0 :close 24}
   :uk100-gbp  {:open 0 :close 20}
   :us2000-usd {:open 0 :close 24}
   :us30-usd   {:open 0 :close 24}})


(s/defn instrument-open-hour
  [instrument :- s/Keyword date-time :- DateTime]
  (let [open-hour (get-in trading-hours-map [instrument :open] 0)]
    (-> date-time
        (dt/to-time-zone (dt/time-zone-for-id "Etc/GMT"))
        (dt/floor dt/day)
        (dt/from-time-zone (dt/time-zone-for-id "Etc/GMT"))
        (dt/plus (dt/hours open-hour)))))


(s/defn instrument-close-hour
  [instrument :- s/Keyword date-time :- DateTime]
  (let [close-hour (get-in trading-hours-map [instrument :close] 24)]
    (-> date-time
        (dt/to-time-zone (dt/time-zone-for-id "Etc/GMT"))
        (dt/floor dt/day)
        (dt/from-time-zone (dt/time-zone-for-id "Etc/GMT"))
        (dt/plus (dt/hours close-hour)))))


(s/defn within-forex-hours?
  [date-time :- DateTime]
  (let [gmt-day-time (-> date-time
                         (dt/to-time-zone (dt/time-zone-for-id "Etc/GMT"))
                         (dt/floor dt/day)
                         (dt/from-time-zone (dt/time-zone-for-id "Etc/GMT")))
        gmt-week-day (dt/day-of-week gmt-day-time)
        week-open-time (-> gmt-day-time
                           (dt/minus (dt/days gmt-week-day))
                           (dt/plus (dt/hours 22)))
        week-close-time (-> gmt-day-time
                            (dt/plus (dt/days (- 5 gmt-week-day)))
                            (dt/plus (dt/hours 22)))]

    (if (dt/before? date-time week-close-time)
      (dt/within? (dt/interval week-open-time week-close-time) date-time)
      (let [next-week-open-time (dt/plus week-open-time (dt/days 7))
            next-week-close-time (dt/plus week-close-time (dt/days 7))]
        (dt/within? (dt/interval next-week-open-time next-week-close-time) date-time)))))


(s/defn within-trading-hours?
  [instrument :- s/Keyword date-time :- DateTime]
  (if (get trading-hours-map instrument)
    (let [gmt-day-time (-> date-time
                           (dt/to-time-zone (dt/time-zone-for-id "Etc/GMT"))
                           (dt/floor dt/day)
                           (dt/from-time-zone (dt/time-zone-for-id "Etc/GMT")))
          gmt-week-day (dt/day-of-week gmt-day-time)
          week-close-time (-> gmt-day-time
                              (dt/plus (dt/days (- 5 gmt-week-day)))
                              (dt/plus (get-in trading-hours-map [instrument :close])))]
      (if (dt/before? date-time week-close-time)
        (dt/within? (dt/interval (instrument-open-hour instrument date-time)
                                 (instrument-close-hour instrument date-time)) date-time)
        (dt/within? (dt/interval (dt/plus date-time (dt/days (- 8 gmt-week-day)))
                                 (dt/plus date-time (dt/days (- 8 gmt-week-day)))) date-time)))
    (within-forex-hours? date-time)))


(-> (dt/now)
    (dt/to-time-zone (dt/time-zone-for-id "America/New_York"))
    (dt/floor dt/day)
    (dt/from-time-zone (dt/time-zone-for-id "America/New_York")))

(s/defn active-markets :- [s/Keyword]
  ([]
    (active-markets (dt/now)))
  ([data-time :- DateTime]
    (cond-> []
            (london-market? data-time) (conj :london)
            (frankfurt-market? data-time) (conj :frankfurt)
            (new-york-market? data-time) (conj :new-york)
            (chicago-market? data-time) (conj :chicago)
            (tokyo-market? data-time) (conj :tokyo)
            (hong-kong-market? data-time) (conj :hong-kong)
            (sydney-market? data-time) (conj :sydney)
            (wellington-market? data-time) (conj :wellington))))
