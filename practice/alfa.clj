(ns euler.practice.alfa)

;;take
(defn take' [a b]
  (if (= (count b) a)
    b
    (take' a (butlast b))))

(defn take'' [a b]
  (loop [la (dec a)
         lb '()]
    (if (= la -1)
      lb
      (recur (dec la) (conj lb (nth b la))))))
    
(defn take$$ [n ls]
  (loop [i 1 res [] raw ls]
        (if (> i n)
            res
            (recur (inc i) 
                   (conj res (first raw))
                   (rest ls)))))
                         

;; Reimplementing Clojure in pure recursion
;; last, butlast, keep, map, take, take-while, remove, drop, drop-while, distinct, range, for
