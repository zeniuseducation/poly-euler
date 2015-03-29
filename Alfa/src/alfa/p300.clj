(ns alfa.p300
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all]))


(def nmax 50)
(def hits 20)

(def score-prob
  (memoize
   (fn [^long k ^long x ^double q]
     (if (== x nmax)
       (cond
        (== k hits) (/ x q)
        (== k (- hits 1)) (- 1 (/ x q))
        :else 0)
       (cond
        (== k hits) (* (/ x q) (score-prob k (+ x 1) q))
        (> k hits) 0
        :else (+ (* (- 1 (/ x q))
                    (score-prob (+ k 1) (+ x 1) q))
                 (* (/ x q)
                    (score-prob k (+ x 1) q))))))))

(defn abs [n] (Math/abs n))

(defn newton
  [target imax start]
  (loop [cur (double start) i (int 0)]
    (if (> i imax)
      cur
      (let [tmp (loop [j 0 prev 0]
                  (let [mj (+ cur (* j (Math/pow 10 (- i))))
                        tmj (- (score-prob 0 1 mj) target)]
                    (if (pos? tmj) (recur (+ j 1) mj) prev)))]
        (recur tmp (+ i 1))))))





