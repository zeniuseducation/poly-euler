(ns alfa.forclojure.one
  (:require [clojure.string :as cs]))

(defn prime?
  [^long p]
  (let [lim (int (Math/sqrt p))]
    (loop [i (int 3)]
      (if (> i lim)
        true
        (if (== 0 (rem p i))
          false
          (recur (+ 2 i)))))))


(defn sum-primes
  [^long lim]
  (->> (range 3 lim 2)
       (filter prime?)
       (reduce + 2)))


(defn t-sum-primes
  [^long lim]
  (transduce
    (comp (filter prime?))
    + 2 (range 3 lim 2)))

(defn sum-sieve
  [^long lim]
  (let [refs (boolean-array (+ lim 1) true)
        llim (long (Math/sqrt lim))]
    (loop [i (long 3) res (long 2)]
      (if (> i lim)
        res
        (if (aget refs i)
          (if (<= i llim)
            (do (loop [j (long (* i i))]
                  (if (> j lim)
                    nil
                    (do (aset refs j false)
                        (recur (+ j i i)))))
                (recur (+ i 2) (+ res i)))
            (recur (+ i 2) (+ res i)))
          (recur (+ i 2) res))))))

(defn sieve-1
  [^long lim]
  (let [refs (boolean-array (+ lim 1) true)
        llim (int (Math/ceil (Math/sqrt lim)))]
    (do (doseq [i (range 3 llim 2)
                :when (aget refs i)]
          (doseq [j (range (* i i) lim (* 2 i))]
            (aset refs j false)))
        (transduce (filter #(aget refs %))
                   + 2 (range 3 lim 2)))))

(defn sieve-2
  [^long lim]
  (let [refs (boolean-array (+ lim 1) true)
        llim (int (Math/ceil (Math/sqrt lim)))
        res (atom 2)]
    (do (doseq [i (range 3 llim 2)
                :when (aget refs i)]
          (do (swap! res + i)
              (doseq [j (range (* i i) lim (* 2 i))]
                (aset refs j false))))
        (doseq [i (if (even? llim)
                    (range (+ llim 1) lim 2)
                    (range llim lim 2))
                :when (aget refs i)]
          (swap! res + i))
        @res)))





