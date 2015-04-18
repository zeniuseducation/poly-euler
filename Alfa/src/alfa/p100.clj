(ns alfa.p100
  (:require
   [clojure.set :refer [union difference intersection subset?]]
   [clojure.core.reducers :as r]
   [clojure.string :as cs]
   [alfa.common :refer :all]))

(defn sol52
  [^long start]
  (loop [i (int start)]
    (let [lst (map #(* i %) (range 2 7))]
      (if (every? #(= % (sort (numcol i)))
                  (map #(sort (numcol %)) lst))
        i
        (recur (+ i 1))))))

(defn pascalim
  [^long lim ^long llim]
  (take lim (iterate #(vec (take-while (fn [x] (<= x llim)) (cons 1 (map +' % (conj (vec (rest %)) 0))))) [1 1])))

(defn sol53
  [^long lim]
  (->> (pascalim lim 1000000)
       (drop 23)
       (map butlast)
       (map count)
       (reduce +)
       (* 2)
       (- (+ 2 (triangle (+ 1 lim))) (triangle 24))))

(def bahan107
  (->> (cs/split-lines (slurp "resources/p107.txt"))
       (map #(cs/split % #","))
       (mapv #(mapv (fn [x] (if (= "-" x) 0 (read-string x))) %))))

(def limb (- (count bahan107) 1))

(def path
  (memoize
   (fn [a b]
     (let [tmp (get-in bahan107 [a b])]
       (if (pos? tmp)
         [tmp [a b]]
         (cond (== a (- b 1))
               (== b limb)
               :else (let [[ja jb] (path (+ a 1) b)]
                       [(+ ja (get-in bahan107 [a (+ a 1)]))
                        (cons a jb)])))))))

(defn sol107
  [bahan]
  (let [matrix (into-array (map into-array bahan))
        lima (count matrix)
        limb (count (aget matrix 1))
        path (fn path [a b visits]
               (let [tmp (get-in matrix [a b])]
                 (if (pos? tmp)
                   [tmp [a b]]
                   (let [[ja jb] (->> (range lima)
                                      (remove visits)
                                      (map #(path % b (conj visits %)))
                                      (min-by first))]
                     [ja (cons a jb)]))))]
    (path 10 34 #{10 34})))


(defn ^long sol87
  [^long lim]
  (let [limsqrt (int (Math/sqrt lim))
        prime-sqr (sieve limsqrt)
        prime-cube (sieve (int (Math/cbrt lim)))
        prime-quad (sieve (int (Math/sqrt limsqrt)))
        res (atom #{})]
    (do (doseq [i prime-sqr]
          (doseq [j prime-cube]
            (doseq [k prime-quad
                    :let [tmp (+ (* i i) (* j j j) (* k k k k))]
                    :while (< tmp lim)]
              (swap! res conj tmp))))
        (count @res))))

(defn ^long sieve-doseq
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        refs (boolean-array (+ lim 1) true)
        res (atom (int 2))]
    (do (doseq [i (range 3 (+ lim 1) 2)]
          (when (aget refs i)
            (if (<= i llim)
              (do (doseq [j (range (* i i) lim (* 2 i))]
                    (aset refs j false))
                  (swap! res + i))
              (swap! res + i))))
        @res)))

(defn ^long sieve-asli
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        refs (boolean-array (+ lim 1) true)]
    (loop [i (int 3) res (int 2)]
      (if (> i lim)
        res
        (if (aget refs i)
          (if (<= i llim)
            (do (loop [j (int (* i i))]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j (* 2 i))))))
                (recur (+ i 2) (+ res i)))
            (recur (+ i 2) (+ res i)))
          (recur (+ i 2) res))))))



