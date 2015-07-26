(ns alfa.under200.five
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

(defn ^long sol173
  [^long lim]
  (loop [i (int 1) res (int 0)]
    (let [isqr (* i i)
          t (- (* (+ i 2) (+ i 2)) isqr)]
      (if (> t lim)
        res
        (recur (+ i 1)
               (+ res (loop [j (int (+ i 2)) resj (int 0)]
                        (let [jsqr (* j j)
                              t (- jsqr isqr)]
                          (if (> t lim)
                            resj
                            (recur (+ j 2) (+ resj 1)))))))))))

(defn ^long sol174
  [^long lim]
  (let [refs (int-array (+ lim 1) 0)]
    (do (loop [i (int 1)]
          (let [isqr (* i i)
                t (- (* (+ i 2) (+ i 2)) isqr)]
            (when (<= t lim)
              (loop [j (int (+ i 2)) ]
                (let [jsqr (* j j)
                      t (- jsqr isqr)]
                  (when (<= t lim)
                    (aset refs t (+ (aget refs t) 1))
                    (recur (+ j 2)))))
              (recur (+ i 1)))))
        (loop [i (int 4) res (int 0)]
          (if (> i lim)
            res
            (if (<= 1 (aget refs i) 10)
              (recur (+ i 4) (+ res 1))
              (recur (+ i 4) res)))))))

(defn jumlah-faktor
  [^long lim]
  (let [faks (int-array (+ lim 1) 1)
        llim (int (Math/sqrt lim))]
    (do (doseq [i (range 2 (+ llim 1))]
          (let [isqr (* i i)]
            (doseq [j (range (* 2 i) (+ lim 1) i)
                    :when (<= isqr j)]
              (if (== (* i i) j)
                (aset faks j (+ i (aget faks j)))
                (aset faks j (+ i (quot j i) (aget faks j)))))))
        (sequence
          (comp (map #(vector % (aget faks %)))
                (filter #(let [[a b] %] (> b a)))
                (map first))
          (range 2 (+ lim 1))))))

(defn sol23
  [^long lim]
  (let [tmp (int-array (jumlah-faktor lim))
        cnt (count tmp)
        abuns (boolean-array (+ lim 1) false)]
    (do (doseq [i (range cnt)
                :let [itmp (aget tmp i)]]
          (doseq [j (range i cnt)
                  :let [jtmp (aget tmp j)
                        stmp (+ itmp jtmp)]
                  :while (<= stmp lim)]
            (aset abuns stmp true)))
        (->> (range 12 (+ lim 1))
             (filter #(aget abuns %))
             (reduce +)
             (- (reduce + (range (+ lim 1))))))))
