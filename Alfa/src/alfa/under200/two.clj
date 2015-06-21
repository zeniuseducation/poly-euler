(ns alfa.under200.two
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs])
  (:import (clojure.lang PersistentVector PersistentList)))

;; Problem 150

(defonce triangles (reductions + (iterate inc 1)))

(defn v2i
  "It returns the array index of a particular place in triangle"
  [[i j]]
  (+ j (triangle i)))

(defn i2v
  "It returns the triangle position for an array index"
  [n]
  (let [nm (take-while #(<= % n) triangles)]
    [(count nm) (if (empty? nm) 0 (- n (last nm)))]))

(defn sol150c
  [lim]
  (let [refs (make-array PersistentList (triangle lim))
        mat (let [tar (expt 2 20)
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
                         (+ i 1)))))]
    ;; We need to fill up the array first
    (do (loop [i (int (- lim 1))]
          (when (pos? i)
            (if (== i (- lim 1))
              (do (doseq [j (map #(vector i %) (range lim))]
                    (aset refs (v2i j) (list (get-in mat j))))
                  (recur (- i 1)))
              (do (doseq [j (map #(vector i %) (range (inc i)))]
                    (aset refs (v2i j)
                          (let [value (get-in mat j)
                                [a b] j
                                left (aget refs (v2i [(inc a) b]))
                                right (aget refs (v2i [(inc a) (inc b)]))]
                            (into '() (cons value (map #(+' value %1 %2) left right))))))
                  (println i)
                  (recur (- i 1))))))
        ;; Only then we can get the minimum sub triangle
        (loop [i (int 0) res (bigint 0)]
          (if (>= i lim)
            res
            (do (println i res)
                (recur (+ i 1) (min res (apply min (aget refs (+ i 1)))))))))))

(defn sol150b
  [lim]
  (let [mat (let [tar (expt 2 20)
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
                         (+ i 1)))))]
    (loop [i (int (- lim 1)) res [] num (int 0)]
      (if (neg? i)
        num
        (if (== i (- lim 1))
          (let [nres (mapv #(vector (get-in mat [i %])) (range lim))]
            (recur (- i 1) nres (min num (apply min (map #(apply min %) nres)))))
          (do (println i)
              (let [nres (mapv #(let [value (get-in mat [i %])]
                                 (->> (map (fn [a b] (+' value a b))
                                           (res %)
                                           (res (+ % 1)))
                                      (cons value)))
                               (range (+ i 1)))]
                (recur (- i 1)
                       nres
                       (min num (apply min (map #(apply min %) nres)))))))))))

;; runs in 45sec
(defn sol150
  [lim]
  (let [mat (let [tar (expt 2 20)
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
                         (+ i 1)))))]
    (loop [i (int (- lim 1)) res [] num (long 99999999)]
      (if (neg? i)
        num
        (if (== i (- lim 1))
          (let [nres (mapv #(hash-map :all [(get-in mat [i %])]
                                      :right [(get-in mat [i %])])
                           (range lim))]
            (do (println num)
                (recur (- i 1) nres (apply min (map #(first (:all %)) nres)))))
          (let [nres (mapv #(let [value (get-in mat [i %])]
                             (hash-map :all (->> (map (fn [a b] (+ value a b))
                                                      (:all (res %))
                                                      (:right (res (+ % 1))))
                                                 (cons value)
                                                 vec)
                                       :right (->> (map (fn [a] (+ value a))
                                                        (:right (res (+ % 1))))
                                                   (cons value)
                                                   vec)))
                           (range (+ i 1)))]
            (do (println i)
                (recur (- i 1) nres (->> (map #(apply min (:all %)) res)
                                         (apply min)
                                         (min num))))))))))










