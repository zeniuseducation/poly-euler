(ns poly-euler.next.p93
  (:require [clojure.set :refer [union difference intersection]]))

(def ops [+ / - *])

(defn insert
  [elm idx xs]
  (concat (take (dec idx) xs)
          [elm]
          (drop (dec idx) xs)))

(defn permute
  [ls]
  (if (= 1 (count ls))
    [[(first ls)]]
    (for [i (range (count ls))
          prest (permute (rest ls))]
      (concat (take i prest) [(first ls)] (drop i prest)))))

(defn permutations
  [ls]
  (if (= 1 (count ls))
    (map vector ls)
    (for [mat ls
          pres (permutations (remove #{mat} ls))]
      (cons mat pres))))

(defn combinations
  [n xs]
  (if (== n 1)
    (map hash-set xs)
    (set (apply concat
                (for [x xs]
                  (map #(conj % x)
                       (combinations (dec n)
                                     (remove #{x} xs))))))))

(defn k-permutations
  [xs n]
  (mapcat permutations (combinations n xs)))

(defn choices
  [xs ops]
  (if (= 1 (count xs))
    (first xs)
    ))


