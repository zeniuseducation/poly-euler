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
