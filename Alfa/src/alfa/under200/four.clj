(ns alfa.under200.four
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :refer [split-lines]]
   [alfa.common :refer :all]
   [clojure.string :as cs]))

(defn palin?
  [^long i]
  (let [n (numcol i)]
    (= n (reverse n))))

(def maxi
  (memoize
   (fn [^long i ^long j]
     (if (or (< i 950) (< j 950))
       0
       (let [res (let [resi (* i j)]
                   (if (palin? resi) resi 0))]
         (max res (maxi i (- j 1)) (maxi (- i 1) (- i 2))))))))

(defn lamina
  [lim odd-even?]
  (let [oe (if odd-even? 3 4)]
    (count (for [i (map #(* % %) (range oe lim 2))
                 j (map #(* % %) (range (- i 2) (- oe 2) -2))
                 :let [diff (- i j)]
                 :while (<= diff lim)]
             diff))))

(defn ^long sol173
  [^long lim]
  (let [llim (int (quot (+ lim 4) 4))]
    (loop [i (int 3) res (int 0)]
      (if (> i llim)
        res
        (recur (+ i 1)
               (+ res
                  (let [is (* i i)]
                    (loop [j (int (- i 2)) resj (int 0)]
                      (let [diff (- is (* j j))]
                        (if (or (> diff lim)
                                (if (odd? i)
                                  (== j -1)
                                  (== j 0)))
                          resj
                          (recur (- j 2)
                                 (+ 1 resj))))))))))))

(defn redux
  ([f [x & xs]]
     (redux f x xs))
  ([f x [l & ls]]
     (if l
       (let [acc (f x l)]
        (cons acc (lazy-seq (redux f acc ls))))
       [])))
