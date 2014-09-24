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

;;butlast
(defn butlast' [col]
  (if (<= (count col) 1)
    nil
    (cons (first col) (butlast' (rest col)))))

(defn butlast'' [col]
  (loop [i col
         res '()]
    (if (= (count i) 1)
      (reverse res)
      (recur (rest i) (cons (first i) res)))))
    
(defn butlast$$ [col]
  (loop [ls col res []]
        (if (= 1 (count ls))
            res
            (recur (rest ls) (conj res (first ls))))))

;;keep
(defn keep' [f col]
  (if (= (count col) 0)
    '()
    (cons (f (first col)) (keep' f (rest col)))))
 
(defn keep'' [f col]
  (loop [i col
         res []]
    (if (= (count i) 0)
      res
      (recur (rest i) (conj res (f (first i)))))))

;;take-while
(defn take-while'' [f col]
  (loop [i col
         res []]
    (if (or (= (count i) 0) (= (f (first i)) false))
      res
      (recur (rest i) (conj res (first i))))))
    
(defn take-while$
  [f col]
  (loop [ls col res []]
        (if (empty? ls) 
            res
            (if (f (first ls)) 
                (recur (rest ls) (conj res (first ls)))
                res))))

;;remove
(defn remove' [f col]
  (if (= (count col) 0)
    '()
    (concat (if-not (f (first col)) (list (first col)) '()) (remove' f (rest col)))))

(defn remove'' [f col]
  (loop [i col
         res '()]
    (if (= (count i) 0)
      res
      (recur (rest i) (concat res (if-not (f (first i)) (list (first i)) '()))))))
    
;;drop-while
(defn drop-while' [f col]
  (if (= (count col) 0)
    []
    (if (f (first col))
      (drop-while' f (rest col))
      col)))

(defn drop-while'' [f col]
  (loop [ls col
         res []]
    (if (zero? (count ls))
      res
      (if (f (first ls))
        (recur (rest ls) res)
        ls))))

    
;; Reimplementing Clojure in pure recursion
;; last, butlast, keep, map, take, take-while, remove, drop, drop-while, distinct, range, for
