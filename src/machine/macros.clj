(ns machine.macros)

(defmacro c-for
  "C-like loop with nested loops support"
  [loops & body]
  (letfn [(c-for-rec [loops body-stmts]
                     (if (seq loops)
                       (let [[var init check next] (take 4 loops)]
                         `((loop [~var ~init]
                             (when ~check
                               ~@(c-for-rec (nthrest loops 4) body-stmts)
                               (recur ~next)))))
                       body-stmts))]
    `(do ~@(c-for-rec loops body) nil)))


(defmacro forloop [[init test step] & body]
  `(loop [~@init]
     (when ~test
       ~@body
       (recur ~step))))


(defmacro uncounted [k quotes]
  `(.max js/Math 0 (.lastIndexOf (js-map #(aget % ~k) ~quotes) js/undefined)))