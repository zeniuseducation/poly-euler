(ns clein.edx
  (:require [clojure.string :as cs]))

(defn cslurp
  [fname]
  (read-string (slurp fname)))

(def sample "asdasd\r\nasdadssd\r\nadssdsd\r\n")

(def dfile "resources/IntegerArray.txt")

(def rawmat (cs/split (slurp dfile) #"\r\n"))

(def ls (map read-string rawmat))

(def mls (sort-by second (map #(vector %2 %1) ls (iterate inc 1))))

(defn sum [xs] (reduce + xs))

(defn inversion
  [lx ls]
  (let [[idx num] lx
        qty (- idx num)]
    (if (neg? qty)
      0
      (+ qty (->> (take (dec num) ls)
                  (filter #(> (first %) idx))
                  count)))))

(defn solution
  [ls]
  (->> ls (map #(inversion % ls)) sum time))

(defn sol1
  [ls]
  (loop [lls ls res 0]
    (if (empty? lls)
      res
      (recur (rest lls)
             (+ res (-> (partial > (first lls))
                        (filter (rest lls))
                        count))))))

(defn merge'
  [l1 l2 res]
  (cond
   (empty? l1) (concat res l2)
   (empty? l2) (concat res l1)
   :else (let [[x & rl1] l1 [y & rl2] l2]
           (if (< x y)
             (merge' rl1 l2 (concat res [x]))
             (merge' l1 rl2 (concat res [y]))))))

(defn msort
  [xs]
  (if (= 1 (count xs))
    xs
    (let [counter (count xs)
          splitter (quot counter 2)]
      (if (odd? counter)
        (merge' (msort (take (inc splitter) xs))
                (msort (drop (inc splitter) xs))
                [])
        (merge' (msort (take splitter xs))
                (msort (drop splitter xs))
                [])))))




