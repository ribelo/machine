(ns machine.strategy.schema
  (:require [schema.core :as s])
  (:import [org.joda.time DateTime]))

(s/defschema TradeSchema
  {(s/optional-key :strategy)         s/Keyword
   :instrument                        s/Keyword
   :period                            s/Keyword
   :side                              (s/enum :buy :sell)
   :open-time                         DateTime
   :open-price                        s/Num
   :stop-loss-price                   s/Num
   (s/optional-key :stop-loss-pips)   s/Num
   :take-profit-price                 s/Num
   (s/optional-key :take-profit-pips) s/Num
   (s/optional-key :lower-bound)      s/Num
   (s/optional-key :upper-bound)      s/Num
   (s/optional-key :trade-size)       s/Num})