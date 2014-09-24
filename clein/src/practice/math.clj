;; Implement these mathematical routines in clojure using pure recursion & loop-recur

(defn factorial [n]
  (if (= n 0)
    1
    (* n (factorial (dec n)))))

(defn fib [nh]
  (if (<= nh 2)
    1
    (+ (fib (dec nh)) (fib (dec (dec nh))))))

(defn sum [col]
  (if (<= (count col) 1)
    (first col)
    (+ (first col) (sum (rest col)))))

(defn prod [col]
  (if (<= (count col) 1)
    (first col)
    (* (first col) (prod (rest col)))))

;; sum of a coll, product of a coll, factorial, fibo, fibolist, prime?, primes-under, primes-range, next-prime, prev-prime
;; maxi of a coll, mini of a coll, factors (returns all factors of an integer).
