(ns alfa.under200.five
  (:require
    [clojure.set :refer [union difference intersection subset?]]
    [clojure.core.reducers :as r]
    [clojure.string :refer [split-lines]]
    [alfa.common :refer :all]
    [clojure.string :as cs]))

(defn ^long sol3
  [^long tar]
  (loop [i (int 3) n tar]
    (cond
      (== i n) i
      (== 0 (rem n i)) (if (odd-prime? i)
                         (let [tmp (loop [m n]
                                     (if (== 0 (rem m i))
                                       (recur (quot m i))
                                       m))]
                           (recur (+ i 2) tmp))
                         (recur (+ i 2) n))
      :else (recur (+ i 2) n))))

(defn ^long sol5
  [^long lim]
  (let [faks (int-array (range (+ lim 1)))]
    (loop [i (int 2) res (int 1)]
      (if (> i lim)
        res
        (let [p (aget faks i)]
          (do (loop [j (int (* i 2))]
                (when (<= j lim)
                  (aset faks j (quot (aget faks j) p))
                  (recur (+ j i))))
              (recur (+ i 1) (* res p))))))))

(defn ^long sol10
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        hlim (if (even? llim) (+ llim 1) (+ llim 2))
        primes (boolean-array (+ lim 1) true)
        res (loop [i (int 3) res (int 2)]
              (if (> i llim)
                res
                (if (aget primes i)
                  (do (loop [j (int (* i i))]
                        (when (<= j lim)
                          (aset primes j false)
                          (recur (+ j i i))))
                      (recur (+ i 2)
                             (+ res i)))
                  (recur (+ i 2) res))))]
    (loop [i (int hlim) resi res]
      (if (> i lim)
        resi
        (if (aget primes i)
          (recur (+ i 2)
                 (+ i resi))
          (recur (+ i 2) resi))))))

(defn ^long sol14b
  [^long lim]
  (let [refs (ref {1 1})
        colat (fn colat [n]
                (if-let [tmp (@refs n nil)]
                  tmp
                  (let [tmpi (if (even? n)
                               (+ 1 (colat (quot n 2)))
                               (+ 1 (colat (+ 1 (* 3 n)))))]
                    (dosync (alter refs assoc n tmpi))
                    tmpi)))]
    (->> [(range 1 200001)
          (range 200001 400001)
          (range 400001 600001)
          (range 600001 800001)
          (range 800001 (+ lim 1))]
         (pmap #(apply max-key colat %))
         (apply max-key colat))))

(defn ^long sol14
  [^long lim]
  (let [refs (int-array (+ lim 1) 0)
        collat (fn collat [^long n]
                 (if (<= n lim)
                   (let [tmp (aget refs n)]
                     (if (== 0 tmp)
                       (let [tmpi (+ 1 (if (even? n)
                                         (collat (quot n 2))
                                         (collat (+ 1 (* 3 n)))))]
                         (aset refs n tmpi)
                         tmpi)
                       tmp))
                   (if (even? n)
                     (+ 1 (collat (quot n 2)))
                     (+ 1 (collat (+ 1 (* 3 n)))))))]
    (aset refs 1 1)
    (loop [i (int 1) cur (int 1) maxi (int 1)]
      (if (> i lim)
        [cur maxi]
        (let [tmp (collat i)]
          (if (> tmp maxi)
            (recur (+ i 1) i tmp)
            (recur (+ i 1) cur maxi)))))))



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
              (loop [j (int (+ i 2))]
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


;; runs in 2.4ms
(defn ^long sol21
  [^long n]
  (let [lim (int (* 3 n))
        llim (int (Math/sqrt lim))
        faks (int-array (+ lim 1) 1)]
    (loop [i (int 2)]
      (when (<= i llim)
        (let [isqr (* i i)]
          (aset faks isqr (+ (aget faks isqr) i))
          (loop [j (+ isqr i)]
            (when (<= j lim)
              (aset faks j (+ (aget faks j) i (quot j i)))
              (recur (+ j i))))
          (recur (+ i 1)))))
    (loop [i (int 2) res (int 0)]
      (if (< i n)
        (let [itmp (aget faks i)]
          (if (and (not= i itmp) (== i (aget faks itmp)))
            (recur (+ i 1) (+ res i))
            (recur (+ i 1) res)))
        res))))

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
