(ns alfa.under200.two
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

;; Problem 150

(defonce
  matrix
  (let [tar (expt 2 20)
        modi (expt 2 19)
        mentah (loop [t 0 k (int 0) res []]
                 (if (> k 500500)
                   (rest res)
                   (recur (mod (+' 797807 (*' 615949 t)) tar)
                          (+ k 1)
                          (conj res (- t modi)))))]
    (loop [awal mentah res [] i (int 1)]
      (if (empty? awal)
        res
        (recur (drop i awal)
               (conj res (vec (take i awal)))
               (+ i 1))))))

(defn sub-triangle
  [a b]
  (if (== a 999)
    {:total (get-in matrix [a b])
     :left  0
     :right 0}
    (let [left (sub-triangle (+ a 1) b)
          right (sub-triangle (+ a 1) (+ b 1))]
      {:total (- (+ (get-in matrix a b) (:total left)
                    (:total right))
                 (:right left)) :left (:total left)
       :right (:total right)})))



