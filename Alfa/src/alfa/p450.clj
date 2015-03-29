(ns alfa.p450
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all])
  (:import (clojure.lang BigInt)))

(defn ^long largest-mod
  [^long lim]
  (loop [i (int 1) res []]
    (if (> i lim)
      res
      (recur
       (+ i 1)
       (conj res
             [i (->> (iterate dec (- i 1))
                     (drop-while #(not= % (rem (* % %) i)))
                     first)])))))

(defn lmod
  [lim]
  (loop [i (int 1) res []]
    (if (> i lim)
      res
      (let [start (isqrt (+ 1 (* 4 i (- i 4))))]
        (recur (+ i 1)
               (conj res
                     [i (->> (iterate dec start)
                             (map #(let [x (* % %)]
                                     (quot (+ 1 %) 2)))
                             (drop-while #(not (integer? %)))
                             first)]))))))





