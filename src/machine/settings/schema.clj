(ns machine.settings.schema
  (:require [schema.core :as s]))


(s/defschema StrategySchema
  {:instruments                      [s/Keyword]
   :periods                          [s/Keyword]
   (s/optional-key :rr)              s/Num
   (s/optional-key :min-rr)          (s/maybe s/Num)
   (s/optional-key :trailing-stop?)   (s/maybe (s/either s/Bool s/Keyword))
   (s/optional-key :trailing-profit?) (s/maybe (s/either s/Bool s/Keyword))
   (s/optional-key :break-even?)      (s/maybe (s/either s/Num s/Keyword))
   (s/optional-key :direction)       (s/maybe s/Keyword)
   (s/optional-key :profit-target)   (s/maybe s/Num)
   (s/optional-key :zones)           [s/Keyword]
   (s/optional-key :markets)         [s/Keyword]})


(s/defschema OandaAccountSchema
  {:oanda    {:account-id      s/Num
              :account-type    (s/enum :sandbox :fxpractice :fxtrade)
              :token   s/Str
              :headers {s/Str s/Any}}
   :money    {:max-risk           s/Num
              :max-dd             s/Num
              :min-margin-level   s/Num
              :max-open-trades    s/Num
              :max-spread-percent s/Num}
   :strategy {s/Keyword StrategySchema}})

(s/defschema SettingsSchema
  [OandaAccountSchema])
