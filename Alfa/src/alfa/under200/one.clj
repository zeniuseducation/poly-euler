(ns alfa.under200.one
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

;; Problem 149

(defn generator
  [^long k]
  (- (mod (- (+ (* k k k 300007) 100003) (* k 200003)) 1000000) 500000))

(defonce matrix
  (let [refs (make-array clojure.lang.BigInt 4000001)]
    (do (doseq [i (range 1 56)]
          (aset refs i (bigint (generator i))))
        (doseq [i (range 56 4000001)]
          (aset refs i (bigint
                         (- (mod (+' 1000000
                                     (aget refs (- i 24))
                                     (aget refs (- i 55)))
                                 1000000)
                            500000))))
        (vec (rest (into [] refs))))))

(defonce bahan (vec (map vec (partition 2000 matrix))))

(defn sum
  [xs]
  (reduce +' xs))

(defn max-line
  [xs]
  (->> (partition-by neg? xs)
       (map sum)
       (drop-while neg?)
       (partition-all 2)
       (map sum)
       (partition-by neg?)
       (map sum)
       (filter pos?)
       (#(cond (empty? %) 0
               (== 1 (count %)) (first %)
               :else (apply max %)))))

(defn max-horizontal
  [lim]
  (->> (range lim)
       (map #(nth bahan %))
       (map #(max-line %))
       (apply max)))

(defn max-vertical
  [lim]
  (apply max
         (for [i (range lim)]
           (->> (for [j (range lim)]
                  (get-in bahan [j i]))
                (max-line)))))

(defn bawah
  [lim]
  (apply max
         (apply concat
                (for [i (range lim)]
                  (pvalues (->> (for [j (range i lim)]
                                  (get-in bahan [(- j i) j]))
                                (max-line))
                           (->> (for [j (range i lim)]
                                  (get-in bahan [j (- j i)]))
                                (max-line)))))))

(defn atas
  [lim]
  (apply max
         (for [i (range lim)]
           (apply max
                  (pvalues (->> (for [j (range (- lim i))]
                                  (get-in bahan [(- lim i j 1) j]))
                                (max-line))
                           (->> (for [j (range (- lim i))]
                                  (get-in bahan [(- lim j 1) (+ j i)]))
                                (max-line)))))))

(defn sol149
  [lim]
  (->> ((juxt max-horizontal max-vertical atas bawah) lim)
       (apply max)))









