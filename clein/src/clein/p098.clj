(ns clein.p098
  (:require
   [clojure.set :refer [union difference intersection]]
   [clojure.string :refer [split-lines]]))

(def raw
  (->> (str "[" (slurp "resources/p098_words.txt") "]")
       read-string
       (group-by count)
       (sort-by key)
       vals
       reverse))

(defn anagrams
  [xs]
  (->> (group-by sort xs)
       vals
       (filter #(> (count %) 1))))

(defn find-group
  [xs]
  (->> (map anagrams raw)
       (filter not-empty)))

(defn ^long square [^long x] (int (* x x)))

(def expt
  (memoize
   (fn [^long a ^long b]
     (cond
      (== b 0) 1
      (== b 1) a
      (even? b) (square (expt a (quot b 2)))
      :else (* (expt a (quot b 2)) (expt a (inc (quot b 2))))))))

(defn find-squares
  [xs]
  (let [counter (dec (count (first xs)))]
    (->> (range)
         (map square)
         (drop-while #(< % (expt 10 counter)))
         (take-while #(< % (expt 10 (inc counter))))
         (map str)
         anagrams
         (sort-by last))))

(defn find-first
  [words squares]
  (loop [[x & xs] (reverse squares) res []]
    (if x
      (let [resi (loop [[y & ys] x resy []]
                   (if y
                     (let [lookup1 (into {} (map vector (first words) y))
                           result1 (reduce #(str %1 (lookup1 %2)) ""
                                           (second words))] 
                       (if (== (count (vals lookup1))
                               (count (distinct (vals lookup1))))
                         (if (some #{result1} x)
                           (recur ys (conj resy result1))
                           (recur ys resy))))
                     resy))]
        (recur xs (apply conj res resi))) 
      res)))

(defn euler98
  [raw]
  (let [all-words (find-group raw)]
    (loop [[x & xs] all-words res []]
      (if (and x (empty? res))
        (let [squares (find-squares (first x))
              resi (loop [[y & ys] x resy []]
                     (if (and y (empty? resy))
                       (recur ys (apply conj resy (find-first y squares)))
                       resy))]
          (recur xs (apply conj res resi)))
        (first res)))))





