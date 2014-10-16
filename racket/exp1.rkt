#lang clojure

(defn square [x] (* x x))

(defn prime?
  [p]
  (if (<= p 20) 
      (if (member p [2 3 5 7 11 13 17 19]) 
          true
          false)
      (if (even? p)
          false 
          (let [lim (sqrt p)]
            (loop [i 3]
                  (if (> i lim)
                      true
                      (if (zero? (remainder p i))
                          false 
                          (recur (+ i 2)))))))))

(defn next-prime
  [p]
  (if (<= p 1)
      2
      (if (= p 2)
          3
          (if (even? p)
              (next-prime (inc p))
              (loop [i p]
                    (if (prime? i)
                        i
                        (recur (+ i 2))))))))

(defn primes-under 
  [lim]
  (if (<= lim 10)
      [2 3 5 7]
      (loop [i 2 res []]
            (if (> i lim)
                res 
                (recur (next-prime i)
                       (cons i res))))))
