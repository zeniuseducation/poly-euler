(ns alfa.p250
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]))

(defn geo-arit
  [r n]
  (transduce
    (map #(* (- 900 (* 3 %))
             (Math/pow r (- % 1))))
    + (range 1 (+ n 1))))

(defn sol250
  [lim]
  (let [modi (expt 10 16)]
    (loop [i (int 2) res [0 1] ctr 0]
      (if (> i lim)
        ctr
        (let [tmp (modex i i modi)
              nres (map #(rem (+ tmp %) modi) res)]
          (recur (+ i 1)
                 (concat res nres)
                 (+ ctr (count (filter #(== 0 (rem % 250)) nres)))))))))

(def modi (expt 10 16))


(def fsol249
  (memoize
    (fn [n]
      (if (== n 37)
        [1 #{0 37}]
        (let [[a b] (fsol249 (next-prime n))
              nres (set (map #(rem (*' % n) modi) b))]
          [(+ a (count (filter prime? nres)))
           (union nres b)])))))


(defn wall
  [[x & xs :as ls]]
  (cond
    (== x 3) [(cons 0 ls)]
    (== x 2) [(cons 0 ls)]
    (== x 1) []
    (== x 0) ls
    :else (let [mpa (wall (cons (- x 2) ls))
                mpb (wall (cons (- x 3) ls))]
            (if (empty? mpa)
              mpb
              (if (empty? mpb)
                mpa
                (concat mpa mpb))))))

(def walls (mapv #(rest (butlast %)) (wall [32])))

(def cracks
  (into {} (map #(vector % (filterv (fn [xs] (not (some (set %) xs))) walls)) walls)))

(def nocrack
  (memoize
   (fn [xs n]
     (if (> n 10)
       1
       (reduce + (map #(nocrack % (+ n 1)) (get cracks xs)))))))

(defn sol215
  []
  (reduce + (map #(nocrack % 2) walls)))








