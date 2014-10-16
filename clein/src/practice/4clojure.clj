(ns euler.practice.4clojure)
;;#28
(defn f28 [col] 
  (if (empty? col)
    '()
    (if (coll? (first col))
      (cons (first (first col)) (f28 (rest col)))
      (if (coll? (first (rest col)))
        (cons (first (rest col)) (f28 (rest col)))
        (cons (first col) (f28 (rest col)))))))

