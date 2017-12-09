(ns machine.router.core
  (:require [clojure.core.async :refer [chan pub unsub close!] :as async]
            [mount.core :refer [defstate]]
            [machine.data-management.core :as dm]))


(declare channel)
(defstate channel
          :start (chan)
          :stop (close! channel))


(defn- event-dispatcher
  [event]
  (condp = event
    :update-account-info (async/buffer 1)
    :update-accounts-info (async/buffer 1)
    :update-instruments-info (async/buffer 1)
    :update-prices-info (async/sliding-buffer 1)
    :new-tick (async/sliding-buffer (* (count dm/instruments) (count dm/periods)))
    :new-candle (async/sliding-buffer (* (count dm/instruments) (count dm/periods)))
    :update-quotes (async/buffer (* (count dm/instruments) (count dm/periods)))
    :analyse/quotes (async/buffer (* (count dm/instruments) (count dm/periods)))
    :analyse/fvb (async/buffer (* (count dm/instruments) (count dm/periods)))
    :analyse/period (async/buffer (count dm/periods))
    :analyse/correlation (* (count dm/instruments) (count dm/periods))
    :analyse/vtr (async/buffer (* (count dm/instruments) (count dm/periods)))

    :open-trade (async/buffer 1)
    :close-trade (async/buffer 1)
    :modify-trade (async/buffer 1)
    (async/buffer 1)))


(declare publication)
(defstate publication
          :start (pub channel :event event-dispatcher)
          :stop (unsub publication channel :event))