(ns euler.practice.alfa)

;;take
(defn take' [n col]
  (if (= (count col) n)
    col
    (take' n (butlast col))))
  
(defn take'' [n col]
  (loop [i (dec n)
         res '()]
    (if (= i -1)
      res
      (recur (dec i) (conj res (nth col i))))))
    
(defn take$$ [n ls]
  (loop [i 1 res [] raw ls]
        (if (> i n)
            res
            (recur (inc i) 
                   (conj res (first raw))
                   (rest ls)))))
                         

;; Reimplementing Clojure in pure recursion
;; last, butlast, keep, map, take, take-while, remove, drop, drop-while, distinct, range, for
