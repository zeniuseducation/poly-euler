(ns clein.p114
  (:require
   [clojure.set :refer [union difference intersection]]
   [clojure.string :as cs]))

(defn valid?
  [xs]
  (if (every? #{1} xs)
    true
    (let [this (butlast (partition-by identity xs))]
      (every? #(if (= 1 (first %))
                 (>= (count %) 3)
                 true)
              this))))

(defn valid-dong?
  [xs]
  (if (#{1} (first xs))
    true
    (let [tmp (drop-while #{0} xs)]
      (if (empty? tmp)
        true 
        (>= (count (take-while #{1} tmp)) 3)))))

(defn gen
  [^long n]
  (loop [i (int 0) res ['()]]
    (if (== i n)
      (-> #(if (= 1 (first %))
             (every? #{1} [(second %) (nth % 2)])
             true)
          (filter res)
          count)
      (recur (inc i)
             (eduction
              (comp (mapcat #(list (cons 1 %)
                                   (cons 0 %)))
                    (filter valid-dong?))
              res)))))

(def raw
  (->> (slurp "resources/sudoku.txt")
       (cs/split-lines)
       (partition 10)
       (map rest)
       (map #(map vec %))
       (pmap #(map (fn [x] (vec (map (fn [k] (read-string (str k))) x))) %))))

(defn traw [n]
  (->> (nth raw (dec n))
       (pmap #(vec (map (fn [x] (if (pos? x) x nil)) %)))))

(defn box [xs n]
  (let [sbahan (take 3 (drop (* 3 (quot (dec n) 3)) xs))]
    (condp = (rem n 3)
      1 (mapcat #(take 3 %) sbahan)
      2 (mapcat #(take 3 (drop 3 %)) sbahan)
      0 (mapcat #(drop 6 %) sbahan))))

(defn row [xs n]
  (nth xs (dec n)))

(defn col [xs n]
  (map #(nth % (dec n)) xs))
