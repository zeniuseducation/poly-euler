(ns alfa.five
  (:require
   [clojure.string :as cs]
   [alfa.common :refer :all]
   [clojure.set :refer [intersection difference]]))

(defn run [f & args]
  (dotimes [i 5]
    (let [res (time (apply f args))]
      (println res))))

(defn ^long p50 [^long lim]
  (let [refs (boolean-array lim true)
        llim (inc (int (Math/sqrt lim)))
        primes (sieve lim)]
    (loop [i (int 2)]
      (if (aget refs i)
        (when (<= i llim)
          (do (loop [j (int (* i i))]
                (when (< j lim)
                  (aset refs j false)
                  (recur (+ j i))))
              (recur (+ i 1))))
        (recur (+ i 1))))
    (->> primes
         (iterate rest)
         (take-while not-empty)
         (mapcat #(->> (iterate butlast %)
                       (take-while not-empty)
                       (keep (fn [x]
                               (let [res (reduce + x)]
                                 (when (< res lim)
                                   [(count x) res]))))))
         (filter #(aget refs (second %)))
         (apply max-key first))))



