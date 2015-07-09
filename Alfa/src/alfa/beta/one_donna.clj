(ns alfa.beta.one-donna)


(def fibo
  (memoize
    (fn [x]
      (cond (== x 0) 1
            (== x 1) 2
            :else (+ (fibo (- x 1))
                     (fibo (- x 2)))))))