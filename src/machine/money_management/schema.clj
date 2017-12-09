(ns machine.money-management.schema
  (:require [schema.core :as s]))


(s/defschema TradeSizeQuerySchema
  {:instrument                           s/Keyword
   :instrument-pip                       s/Num
   :instrument-tick-value                s/Num
   :open-price                           s/Num
   :stop-loss-price                      s/Num
   :drawndown                            s/Num
   :highest-eq                           s/Num
   :risk-reward                          s/Num
   :balance                              s/Num
   :max-risk                             s/Num
   :max-dd                               s/Num
   (s/optional-key :max-trade-units)     s/Num
   (s/optional-key :max-units-available) s/Num
   s/Keyword                             s/Any})
