(ns euler.tutorial
  (:require [clojure.set :refer :all]))

(defn sqr [x] (* x x))
(defn cube [x] (* x x x))

(defn mapsqr
  [ls]
  (keep #(if (even? %) (sqr %)) ls))

(defn filters
  [ls]
  (remove #(zero? (rem % 5)) ls))

(defn removes
  [elmt ls]
  (remove #(= % elmt) ls))

(def bahan [1 2 [2 3] 2 [3 [3 [2 3 4 [2 3]] 4] 2 2 [3 4] 2 3 [2 3] 32] 2 3 ])

(defn map'
  [f ls]
  (if (empty? ls)
    []
    (conj (map' f (rest ls)) (f (first ls)))))

(defn flatten'
  [ls]
  (if (not (coll? ls))
    [ls]
    (mapcat #(flatten' %) ls)))

(defn gcd
  [a b]
  (cond (= a 0) b
        (= b 0) a
        (> a b) (gcd (- a b) b)
        (< a b) (gcd (- b a) a)
        (= a b) a))

(defn lgcd
  [ls]
  (reduce gcd ls))

(defn psqr?
  [n]
  (let [tmp (Math/sqrt n)]
    (== tmp (int tmp))))

(defn triplets [lim]
  (for [a (range 3 (/ lim 4))
        b (range a (/ lim 2))
        :let [asqr (sqr a)
              bsqr (sqr b)
              csqr (+ asqr bsqr)]
        :when (and (psqr? csqr)
                   (<= (+ a b (Math/sqrt csqr)) lim))]
    (let [c (int (Math/sqrt csqr))]
      (+ a b c))))

(defn solution
  [lim]
  (->> (triplets lim)
       (frequencies)
       (map val)
       (reduce #(if (= %2 1) (+ %1 %2) %1))))

(defn another
  [lim]
  (-> (fn [n] (* n n n n))
      (map (range 1 lim))))








