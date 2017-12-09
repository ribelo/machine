(ns machine.oanda.schema
  (:require [schema.core :as s]
            [camel-snake-kebab.core :refer [->PascalCase]]
            [machine.utils :refer :all])
  (:import [clojure.lang Keyword]
           [org.joda.time DateTime]))




(def AccountsResponse
  [{(s/required-key :account-id)       s/Num
    (s/required-key :account-name)     String
    (s/required-key :account-currency) Keyword
    (s/required-key :margin-rate)      s/Num}])



(def AccountResponse
  {(s/required-key :account-id)        s/Num
   (s/required-key :account-name)      String
   (s/required-key :balance)           s/Num
   (s/required-key :unrealized-profit) s/Num
   (s/required-key :realized-profit)   s/Num
   (s/required-key :margin-used)       s/Num
   (s/required-key :margin-available)  s/Num
   (s/required-key :open-trades)       s/Num
   (s/required-key :open-orders)       s/Num
   (s/required-key :account-currency)  Keyword
   (s/required-key :margin-rate)       s/Num})


(def OrderSchema
  {(s/required-key :id)            s/Num
   (s/required-key :instrument)    String
   (s/required-key :units)         s/Num
   (s/required-key :side)          (s/enum :buy :sell)
   (s/required-key :type)          (s/enum :market :market-if-touched :limit :stop)
   (s/required-key :time)          DateTime
   (s/required-key :price)         s/Num
   (s/required-key :take-profit)   s/Num
   (s/required-key :stop-loss)     s/Num
   (s/required-key :expiry)        DateTime
   (s/required-key :uper-bound)    s/Num
   (s/required-key :lower-bound)   s/Num
   (s/required-key :trailing-stop) s/Num})


(def OrdersResponse
  [OrderSchema])


(def TradeSchema
  {(s/required-key :id)              s/Num
   (s/required-key :instrument)      s/Keyword
   (s/required-key :units)           s/Num
   (s/required-key :side)            (s/enum :buy :sell)
   (s/required-key :time)            DateTime
   (s/required-key :price)           s/Num
   (s/required-key :take-profit)     s/Num
   (s/required-key :stop-loss)       s/Num
   (s/required-key :trailing-stop)   s/Num
   (s/required-key :trailing-amount) s/Num})


(def TradesResponse
  [TradeSchema])


(def InstrumentSchema
  {(s/required-key :instrument)        s/Keyword
   (s/required-key :display-name)      s/Str
   (s/required-key :pip)               s/Num
   (s/required-key :max-trade-units)   s/Num
   (s/optional-key :max-trailing-stop) s/Num
   (s/optional-key :min-trailing-stop) s/Num
   (s/optional-key :margin-rate)       s/Num
   (s/optional-key :halted?)           s/Bool})


(def InstrumentsResponse
  [InstrumentSchema])


(def PriceSchema
  {(s/required-key :instrument) s/Keyword
   (s/required-key :time)       DateTime
   (s/required-key :bid)        s/Num
   (s/required-key :ask)        s/Num
   (s/optional-key :halted?)    s/Bool})


(def PricesResponse
  [PriceSchema])

(s/validate (s/cond-pre {s/Keyword s/Keyword}
                        {s/Keyword s/Num}) {:a :a})

(s/defschema QutesHistoryQuery
  (let [base {(s/required-key :instrument)        s/Keyword
              (s/required-key :period)            s/Keyword
              (s/required-key :candleFormat)      s/Str
              (s/optional-key :dailyAlignment)    s/Num
              (s/optional-key :weeklyAlignment)   s/Str
              (s/optional-key :alignmentTimezone) s/Str
              (s/optional-key :includeFirst)      s/Bool}]
    (s/conditional
      #(get % :count) (merge base {(s/required-key :count) s/Num})
      #(and (get % :start) (get % :end)) (merge base {(s/required-key :start) s/Num
                                                      (s/required-key :end)   s/Num}))))


;(def QuotesHistorySchema
;  {(s/required-key :time)      (s/pred #(.isArray %))
;   (s/required-key :open)      doubles
;   (s/required-key :high)      doubles
;   (s/required-key :low)       doubles
;   (s/required-key :close)     doubles
;   (s/required-key :volume)    longs
;   (s/optional-key :complete?) booleans})


(def OrderCreateQuery
  {(s/required-key :instrument)         s/Keyword
   (s/required-key :units)              s/Num
   (s/required-key :side)               (s/enum :buy :sell)
   (s/required-key :type)               (s/enum :market :market-if-touched :limit :stop)
   (s/optional-key :expiry)             DateTime
   (s/optional-key :price)              s/Num
   (s/optional-key :lower-bound)        s/Num
   (s/optional-key :upper-bound)        s/Num
   (s/optional-key :take-profit)        s/Num
   (s/optional-key :stop-loss)          s/Num
   (s/optional-key :trailing-stop-loss) s/Num})


(def OrderModifyQuery
  {(s/optional-key :trade-id)           s/Num
   (s/optional-key :units)              s/Num
   (s/optional-key :price)              s/Num
   (s/optional-key :expiry)             DateTime
   (s/optional-key :lower-bound)        s/Num
   (s/optional-key :upper-bound)        s/Num
   (s/optional-key :take-profit)        s/Num
   (s/optional-key :stop-loss)          s/Num
   (s/optional-key :trailing-stop-loss) s/Num})


(def OrderCreateResponse
  {(s/required-key :instrument)    s/Keyword
   (s/required-key :time)          DateTime
   (s/required-key :price)         s/Num
   (s/required-key :trade-opened)  {(s/required-key :id)            s/Num
                                    (s/required-key :units)         s/Num
                                    (s/required-key :side)          (s/enum :buy :sell)
                                    (s/required-key :take-profit)   s/Num
                                    (s/required-key :stop-loss)     s/Num
                                    (s/required-key :trailing-stop) s/Num}
   (s/optional-key :trade-reduced) {(s/optional-key :id)       s/Num
                                    (s/optional-key :units)    s/Num
                                    (s/optional-key :profit)   s/Num
                                    (s/optional-key :interest) s/Num}
   (s/optional-key :trades-closed) [s/Any]

   (s/optional-key :period)        s/Keyword
   (s/optional-key :strategy)      s/Keyword})


(def OrderModifyResponse
  {(s/required-key :id)                 s/Num
   (s/required-key :time)               DateTime
   (s/required-key :type)               (s/enum :market :market-if-touched :limit :stop)
   (s/required-key :instrument)         s/Keyword
   (s/required-key :side)               (s/enum :buy :sell)
   (s/required-key :units)              s/Num
   (s/required-key :price)              s/Num
   (s/required-key :expiry)             DateTime

   (s/optional-key :lower-bound)        s/Num
   (s/optional-key :upper-bound)        s/Num
   (s/optional-key :take-profit)        s/Num
   (s/optional-key :stop-loss)          s/Num
   (s/optional-key :trailing-stop-loss) s/Num

   (s/optional-key :period)             s/Keyword
   (s/optional-key :strategy)           s/Keyword})


(def OrderCloseResponse
  {(s/required-key :id)         s/Num
   (s/required-key :time)       DateTime
   (s/required-key :instrument) s/Keyword
   (s/required-key :side)       (s/enum :buy :sell)
   (s/required-key :units)      s/Num
   (s/required-key :price)      s/Num

   (s/optional-key :period)     s/Keyword
   (s/optional-key :strategy)   s/Keyword})


(def TradeModifyQuery
  {(s/required-key :trade-id)           s/Num
   (s/optional-key :take-profit)        s/Num
   (s/optional-key :stop-loss)          s/Num
   (s/optional-key :trailing-stop-loss) s/Num})


(def TradeCloseResponse
  {(s/required-key :id)         s/Num
   (s/required-key :time)       DateTime
   (s/required-key :instrument) s/Keyword
   (s/required-key :side)       (s/enum :buy :sell)
   (s/required-key :price)      s/Num
   (s/required-key :profit)     s/Num

   (s/optional-key :period)     s/Keyword
   (s/optional-key :strategy)   s/Keyword})


(def TradeModifyResponse
  {(s/required-key :id)              s/Num
   (s/required-key :time)            DateTime
   (s/required-key :instrument)      s/Keyword
   (s/required-key :side)            (s/enum :buy :sell)
   (s/required-key :units)           s/Num
   (s/required-key :price)           s/Num
   (s/optional-key :take-profit)     s/Num
   (s/optional-key :stop-loss)       s/Num
   (s/optional-key :trailing-stop)   s/Num
   (s/optional-key :trailing-amount) s/Num

   (s/optional-key :period)          s/Keyword
   (s/optional-key :strategy)        s/Keyword})


(def MarketOrderHistorySchema
  {(s/required-key :id)                          s/Num
   (s/required-key :account-id)                  s/Num
   (s/required-key :time)                        DateTime
   (s/required-key :type)                        (s/eq :market-order-create)
   (s/required-key :instrument)                  s/Keyword
   (s/required-key :side)                        (s/enum :buy :sell)
   (s/required-key :units)                       s/Num
   (s/required-key :price)                       s/Num
   (s/required-key :profit)                      s/Num
   (s/required-key :interest)                    s/Num
   (s/required-key :account-balance)             s/Num

   (s/optional-key :lower-bound)                 s/Num
   (s/optional-key :upper-bound)                 s/Num
   (s/optional-key :take-profit-price)           s/Num
   (s/optional-key :stop-loss-price)             s/Num
   (s/optional-key :trailing-stop-loss-distance) s/Num

   (s/optional-key :trade-opened)                {(s/required-key :id)    s/Num
                                                  (s/required-key :units) s/Num}
   (s/optional-key :trade-reduced)               {(s/required-key :id)       s/Num
                                                  (s/required-key :units)    s/Num
                                                  (s/required-key :profit)   s/Num
                                                  (s/required-key :interest) s/Num}

   (s/optional-key :period)                      s/Keyword
   (s/optional-key :strategy)                    s/Keyword})


(def MarketIfTouchedOrderHistorySchema
  {(s/required-key :id)                          s/Num
   (s/required-key :account-id)                  s/Num
   (s/required-key :time)                        DateTime
   (s/required-key :type)                        (s/eq :market-if-touched-order-create)
   (s/required-key :instrument)                  s/Keyword
   (s/required-key :side)                        (s/enum :buy :sell)
   (s/required-key :units)                       s/Num
   (s/required-key :price)                       s/Num
   (s/required-key :expiry)                      DateTime
   (s/required-key :reason)                      s/Any      ;FIXME

   (s/optional-key :lower-bound)                 s/Num
   (s/optional-key :upper-bound)                 s/Num
   (s/optional-key :take-profit-price)           s/Num
   (s/optional-key :stop-loss-price)             s/Num
   (s/optional-key :trailing-stop-loss-distance) s/Num

   (s/optional-key :period)                      s/Keyword
   (s/optional-key :strategy)                    s/Keyword})


(def StopOrderHistorySchema
  {(s/required-key :id)                          s/Num
   (s/required-key :account-id)                  s/Num
   (s/required-key :time)                        DateTime
   (s/required-key :type)                        (s/eq :stop-order-create)
   (s/required-key :instrument)                  s/Keyword
   (s/required-key :side)                        (s/enum :buy :sell)
   (s/required-key :units)                       s/Num
   (s/required-key :price)                       s/Num
   (s/required-key :expiry)                      DateTime
   (s/required-key :reason)                      s/Any      ;FIXME

   (s/optional-key :lower-bound)                 s/Num
   (s/optional-key :upper-bound)                 s/Num
   (s/optional-key :take-profit-price)           s/Num
   (s/optional-key :stop-loss-price)             s/Num
   (s/optional-key :trailing-stop-loss-distance) s/Num

   (s/optional-key :period)                      s/Keyword
   (s/optional-key :strategy)                    s/Keyword})


(def LimitOrderHistorySchema
  (merge StopOrderHistorySchema {(s/required-key :type) (s/eq :limit-order-create)}))


(def OrderUpdateHistorySchema
  {(s/required-key :id)                          s/Num
   (s/required-key :account-id)                  s/Num
   (s/required-key :time)                        DateTime
   (s/required-key :type)                        (s/eq :order-update)
   (s/required-key :instrument)                  s/Keyword
   (s/required-key :side)                        (s/enum :buy :sell)
   (s/required-key :units)                       s/Num
   (s/required-key :price)                       s/Num
   (s/required-key :order-id)                    s/Num
   (s/required-key :reason)                      s/Any      ;FIXME

   (s/optional-key :lower-bound)                 s/Num
   (s/optional-key :upper-bound)                 s/Num
   (s/optional-key :take-profit-price)           s/Num
   (s/optional-key :stop-loss-price)             s/Num
   (s/optional-key :trailing-stop-loss-distance) s/Num

   (s/optional-key :period)                      s/Keyword
   (s/optional-key :strategy)                    s/Keyword})


(def OrderCancelHistorySchema
  {(s/required-key :id)         s/Num
   (s/required-key :account-id) s/Num
   (s/required-key :time)       DateTime
   (s/required-key :type)       (s/eq :order-cancel)
   (s/required-key :order-id)   s/Num
   (s/required-key :reason)     s/Any                       ;FIXME

   (s/optional-key :period)     s/Keyword
   (s/optional-key :strategy)   s/Keyword})


(def OrderFilledHistorySchema
  {(s/required-key :id)                          s/Num
   (s/required-key :account-id)                  s/Num
   (s/required-key :time)                        DateTime
   (s/required-key :type)                        (s/eq :order-filled)
   (s/required-key :instrument)                  s/Keyword
   (s/required-key :side)                        (s/enum :buy :sell)
   (s/required-key :units)                       s/Num
   (s/required-key :price)                       s/Num
   (s/required-key :profit)                      s/Num
   (s/required-key :interest)                    s/Num
   (s/required-key :account-balance)             s/Num
   (s/required-key :order-id)                    s/Num

   (s/optional-key :lower-bound)                 s/Num
   (s/optional-key :upper-bound)                 s/Num
   (s/optional-key :take-profit-price)           s/Num
   (s/optional-key :stop-loss-price)             s/Num
   (s/optional-key :trailing-stop-loss-distance) s/Num

   (s/optional-key :trade-opened)                {(s/required-key :id)    s/Num
                                                  (s/required-key :units) s/Num}
   (s/optional-key :trade-reduced)               {(s/required-key :id)       s/Num
                                                  (s/required-key :units)    s/Num
                                                  (s/required-key :profit)   s/Num
                                                  (s/required-key :interest) s/Num}

   (s/optional-key :period)                      s/Keyword
   (s/optional-key :strategy)                    s/Keyword})


(def TradeUpdateHistorySchema
  {(s/required-key :id)                          s/Num
   (s/required-key :account-id)                  s/Num
   (s/required-key :time)                        DateTime
   (s/required-key :type)                        (s/eq :trade-update)
   (s/required-key :instrument)                  s/Keyword
   (s/optional-key :side)                        (s/enum :buy :sell) ;FIXME
   (s/required-key :units)                       s/Num
   (s/required-key :trade-id)                    s/Num

   (s/optional-key :take-profit-price)           s/Num
   (s/optional-key :stop-loss-price)             s/Num
   (s/optional-key :trailing-stop-loss-distance) s/Num

   (s/optional-key :period)                      s/Keyword
   (s/optional-key :strategy)                    s/Keyword})


(def TradeCloseHistorySchema
  {(s/required-key :id)              s/Num
   (s/required-key :account-id)      s/Num
   (s/required-key :time)            DateTime
   (s/required-key :type)            (s/eq :trade-close)
   (s/required-key :instrument)      s/Keyword
   (s/required-key :units)           s/Num
   (s/required-key :side)            (s/enum :buy :sell)
   (s/optional-key :price)           s/Num                  ;FIXME
   (s/required-key :profit)          s/Num
   (s/required-key :interest)        s/Num
   (s/required-key :account-balance) s/Num
   (s/required-key :trade-id)        s/Num

   (s/optional-key :pips-profit)     s/Num
   (s/optional-key :period)          s/Keyword
   (s/optional-key :strategy)        s/Keyword})


(def TakeProffitFilledHistorySchema
  {(s/required-key :id)              s/Num
   (s/required-key :account-id)      s/Num
   (s/required-key :time)            DateTime
   (s/required-key :type)            (s/eq :take-profit-filled)
   (s/required-key :instrument)      s/Keyword
   (s/required-key :units)           s/Num
   (s/required-key :side)            (s/enum :buy :sell)
   (s/required-key :price)           s/Num
   (s/required-key :profit)          s/Num
   (s/required-key :interest)        s/Num
   (s/required-key :account-balance) s/Num
   (s/required-key :trade-id)        s/Num

   (s/optional-key :pips-profit)     s/Num
   (s/optional-key :period)          s/Keyword
   (s/optional-key :strategy)        s/Keyword})


(def StopLossFilledHistorySchema
  (merge TakeProffitFilledHistorySchema {(s/required-key :type) (s/eq :stop-loss-filled)}))


(def TrailingStopFilledSchema
  (merge TakeProffitFilledHistorySchema {(s/required-key :type) (s/eq :trailing-stop-filled)}))


(def SetMarginRateHistorySchema
  {(s/required-key :id)          s/Num
   (s/required-key :account-id)  s/Num
   (s/required-key :time)        DateTime
   (s/required-key :type)        (s/eq :set-margin-rate)
   (s/required-key :margin-rate) s/Num})


(def TransferFoundsHistorySchema
  {(s/required-key :id)          s/Num
   (s/required-key :account-id)  s/Num
   (s/required-key :time)        DateTime
   (s/required-key :type)        (s/eq :transfer-founds)
   (s/required-key :margin-rate) s/Num})


(def DailyInterestHistorySchema
  {(s/required-key :id)              s/Num
   (s/required-key :account-id)      s/Num
   (s/required-key :time)            DateTime
   (s/required-key :type)            (s/eq :daily-interest)
   (s/required-key :instrument)      s/Keyword
   (s/required-key :interest)        s/Num
   (s/required-key :account-balance) s/Num})


(def FeeHistorySchema
  {(s/required-key :id)              s/Num
   (s/required-key :account-id)      s/Num
   (s/required-key :time)            DateTime
   (s/required-key :type)            (s/eq :fee)
   (s/required-key :amount)          s/Num
   (s/required-key :account-balance) s/Num
   (s/required-key :reason)          s/Any})                ;FIXME


(s/defschema TransactionHistorySchema
  (s/conditional
    #(= :market-order-create (:type %)) MarketOrderHistorySchema
    #(= :market-if-touched-order-create (:type %)) MarketIfTouchedOrderHistorySchema
    #(= :stop-order-create (:type %)) StopOrderHistorySchema
    #(= :limit-order-create (:type %)) LimitOrderHistorySchema
    #(= :order-update (:type %)) OrderUpdateHistorySchema
    #(= :order-cancel (:type %)) OrderCancelHistorySchema
    #(= :order-filled (:type %)) OrderFilledHistorySchema
    #(= :trade-update (:type %)) TradeUpdateHistorySchema
    #(= :trade-close (:type %)) TradeCloseHistorySchema
    #(= :take-profit-filled (:type %)) TakeProffitFilledHistorySchema
    #(= :stop-loss-filled (:type %)) StopLossFilledHistorySchema
    #(= :trailing-stop-filled (:type %)) TrailingStopFilledSchema
    #(= :set-margin-rate (:type %)) SetMarginRateHistorySchema
    #(= :transfer-founds (:type %)) TransferFoundsHistorySchema
    #(= :daily-interest (:type %)) DailyInterestHistorySchema
    #(= :fee (:type %)) FeeHistorySchema))

(def TransactionsResponse
  [TransactionHistorySchema])


;(s/defschema CalendarResponse TODO
;  {(s/required-key unit ())})