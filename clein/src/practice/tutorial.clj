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

(comment
  - take-while
  - filter
  - remove
  - drop-while)

(defn mapthis
  [f l1 l2]
  (map f l1 l2))

(defn forthis
  [lim]
  (for [a (range 1 (inc lim))
        b (range 1 a)
        c (range 1 b)
        :let [bc (+ (sqr b) (sqr c))]
        :when (= (sqr a) bc)]
    [c b a]))

(defn combination
  [ls k]
  (if (= 1 k)
    (map hash-set ls)
    (for [a ls]
      (into #{}
            (mapcat #(union % #{a})
                    (into #{}
                          (combination (removes a ls)
                                       (dec k))))))))






