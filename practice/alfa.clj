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
                 
;;drop
(defn drop' [n col]
  (if (= n 0)
    col
    (drop' (dec n) (rest col))))

(defn drop'' [n col]
  (loop [i n
         res col]
    (if (= i 0)
      res
      (recur (dec i) (rest res)))))
                         
;;last
(defn last' [col]
  (if (<= (count col) 1)
    (first col)
    (last' (rest col))))

(defn last'' [col]
  (loop [i (count col)
         res col]
    (if (<= i 1)
      (first res)
      (recur (dec i) (rest res)))))
    
;;range
(defn range' [i j]
  (if (< i j)
    (if (= i j)
      '()
      (cons i (range' (inc i) j)))))

(defn range'' [i j]
  (loop [n i
         res '()]
    (if (= n j)
      (reverse res)
      (recur (inc n) (cons n res)))))
    
;; Reimplementing Clojure in pure recursion
;; last, butlast, keep, map, take, take-while, remove, drop, drop-while, distinct, range, for
