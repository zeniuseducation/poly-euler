(ns alfa.special.p504
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

(defn abs
  [n]
  (if (pos? n) n (- n)))



(def sum-kuadi
  (memoize
    (fn [a b]
      (->> (for [x (range 1 a) y (range 1 b)
                 :when (< (+ (* b x) (* a y)) (* a b))] 1)
           (count)))))

(defn total
  [m]
  (count (for [i (range 1 (inc m))
               j (range 1 (inc m))
               k (range 1 (inc m))
               l (range 1 (inc m))
               :when (->> (+ (+ (sum-kuadi i j) (dec i))
                             (+ (sum-kuadi j k) (dec j))
                             (+ (sum-kuadi k l) (dec k))
                             (+ (sum-kuadi l i) (dec l)))
                          inc psqr?)] 1)))





