(ns alfa.three
  (:require
   [clojure.set :refer [union difference intersection]]))

(defn prize [n]
  (loop [i 1 [r b] [1 1] res []]
    (if (> i n)
      res
      (recur (inc i)
             [(inc r) 1]
             (let [sum (+ r b)]
               (conj res [(/ b sum) (/ r sum)]))))))


(defn combinations
  [n xs]
  (if (== n 1)
    (map hash-set xs)
    (set (apply concat
                (for [x xs]
                  (map #(conj % x)
                       (combinations (dec n)
                                     (remove #{x} xs))))))))

(defn add-more
  [st xs]
  (let [bahan (difference xs st)]
    (map #(conj st %) bahan)))

(defn combs
  [start n xs]
  (loop [i 1 res (set (map hash-set xs)) all #{}]
    (if (== i n)
      (union res all)
      (let [result (set (mapcat #(add-more % xs) res))]
        (recur (inc i)
               result
               (if (>= i start)
                 (union all res)
                 all))))))

(def prizes (prize 15))

(defn prize-one
  [st]
  (let [ct (count st)]
    (reduce * (map #(if (some #{%} st)
                      (first (nth prizes (dec %)))
                      (second (nth prizes (dec %))))
                   (range 1 16)))))

(defn prize-all
  [start end bahan]
  (->> (combs start end bahan)
       (map prize-one)
       (reduce +)
       (/ 1.0)))








