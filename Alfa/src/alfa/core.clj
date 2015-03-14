(ns alfa.core
  (:require
   [clojure.core.reducers :as r]))

(set! *unchecked-math* true)

(defn ^long prime?
  [^long n]
  (loop [i (int 3)]
    (if (> (* i i) n)
      true
      (if (== 0 (rem n i))
        false
        (recur (+ i 2))))))

(defn ^long sum-primes
  [^long lims ^long parts]
  (let [f (fn [st lim]
            (loop [i (int st) res (long 0)]
              (if (> i lim)
                res
                (recur (+ 2 i)
                       (if (prime? i)
                         (+ res i)
                         res)))))]
    (->> (range (quot lims parts))
         (pmap #(f (+ 1 (* % parts))
                   (* (+ 1 %) parts)))
         (r/fold +))))

(defn ^long sum-sieve
  [^long lim]
  (let [primes (boolean-array (+ 1 lim) true)
        llim (inc (int (Math/sqrt lim)))]
    (loop [i (int 3) res (long 2)]
      (if (> i lim)
        res
        (if (aget primes i)
          (do (when (<= i llim)
                (loop [j (long (* i i))]
                  (if (> j lim)
                    nil
                    (do (aset primes j false)
                        (recur (+ j (* i 2)))))))
              (recur (+ i 2) (+ res i)))
          (recur (+ i 2) res))))))

(defn ^long sum-sieve2
  [^long lim]
  (let [primes (boolean-array (+ 1 lim) true)
        llim (inc (int (Math/sqrt lim)))]
    (loop [i (int 3) res (long 2)]
      (if (>= i llim)
        (+ res
           (r/fold + (r/filter #(aget primes %) (range i lim 2))))
        (if (aget primes i)
          (do (loop [j (long (* i i))]
                (if (> j lim)
                  nil
                  (do (aset primes j false)
                      (recur (+ j (* i 2))))))
              (recur (+ i 2) (+ res i)))
          (recur (+ i 2) res))))))

(defmacro defm [nama binding body]
  `(def ~nama (memoize (fn ~binding ~body))))

(def expt
  (memoize
   (fn [^long a ^long m]
     (cond (== m 0) 1
           (== m 1) a
           (even? m) (let [k (expt a (quot m 2))]
                       (*' k k))
           :else (let [k (expt a (quot m 2))]
                   (*' k k a))))))

(defn fibo
  [tar]
  (loop [a 1 b 0 i (long 1)]
    (if (> a tar) i (recur (+' a b) a (+ 1 i)))))

(defn ^long sol12 
  [^long tar]
  (let [cdiv (fn [^long x]
               (if (== 0 (rem x 2))
                 (loop [i (int 2) res (int 2)]
                   (if (>= (* i i) x)
                     (if (> (* i i) x) res (+ 1 res))
                     (recur (+ i 1)
                            (if (== 0 (rem x i))
                              (+ res 2)
                              res))))
                 (loop [i (int 3) res (int 2)]
                   (if (>= (* i i) x)
                     (if (> (* i i) x) res (+ 1 res))
                     (recur (+ i 2)
                            (if (== 0 (rem x i))
                              (+ res 2)
                              res))))))]
    (loop [i (int 10)]
      (let [fact (if (== 0 (rem i 2))
                   (* (cdiv (quot i 2))
                      (cdiv (+ i 1)))
                   (* (cdiv (quot (+ i 1) 2))
                      (cdiv i)))]
        (if (> fact tar) (quot (* i (+ i 1)) 2) (recur (+ i 1)))))))

(defn ^boolean abun?
  [^long m]
  (if (even? m)
    (loop [i (int 2) res (int 1)]
      (cond
       (> res m) true
       (> (* i i) m) false
       (== (* i i) m) (> (+ res i) m)
       (zero? (rem m i)) (recur (+ 1 i)
                                (+ res i (quot m i)))
       :else (recur (+ 1 i) res)))
    (loop [i (int 3) res (int 1)]
      (cond
       (> res m) true
       (> (* i i) m) false
       (== (* i i) m) (> (+ res i) m)
       (zero? (rem m i)) (recur (+ 2 i)
                                (+ res i (quot m i)))
       :else (recur (+ 2 i) res)))))

(defn ^long non-abundant-sum
  [^long lim]
  (let [abuns (boolean-array lim)
        sum-abuns (boolean-array lim)]
    (do (loop [i (int 12)]
          (if (< i lim)
            (recur (do (if (abun? i)
                         (aset abuns i true))
                       (+ 1 i)))
            i))
        (loop [i (int 12)]
          (if (< i (quot lim 2))
            (recur (do (if (aget abuns i)
                         (loop [j (int i)]
                           (if (< (+ i j) lim)
                             (recur (do (if (aget abuns j)
                                          (aset sum-abuns (+ i j) true))
                                        (+ 1 j))))))
                       (+ 1 i)))
            i))
        (->> (range 1 lim)
             (r/filter #(not (aget sum-abuns %)))
             (r/fold +)))))
