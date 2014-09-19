(ns euler.practice.alfa)

;;take''
(defn a-take [a b]
  (loop [la (dec a)
         lb '()]
    (if (= la -1)
      lb
      (recur (dec la) (conj lb (nth b la))))))

;; Reimplementing Clojure in pure recursion
;; last, butlast, keep, map, take, take-while, remove, drop, drop-while, distinct, range, for
