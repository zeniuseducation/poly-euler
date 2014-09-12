(ns poly-euler.earlyprobs)

(defn euler1
  "Returns the sum of all multiplies of a or b which less than lim"
  [a b lim]
  (reduce + (filter #(or (zero? (rem % a))
                         (zero? (rem % b)))
                    (range 1 lim))))

;; poly-euler.earlyprobs> (time (euler1 3 5 1000))
"Elapsed time: 3.394 msecs"

(defn euler2
  [[a b] lim res]
  (if (>= a lim)
    res
    (euler2 [(+ a b) a]
            lim
            (if (even? a)
              (+ res a)
              res))))

;; poly-euler.earlyprobs> (time (euler2 [1 2] 4000000 0))
"Elapsed time: 0.08507 msecs"

;; Problem no 3

;; first the classic prime?

(defn prime?
  [p]
  (cond (<= p 20) (if (some #(= % p) [2 3 5 7 11 13 17 19]) true false)
        (even? p) false
        :else (let [lim (inc (Math/sqrt p))]
                (loop [i 3]
                  (if (> i lim)
                    true
                    (if (zero? (rem p i))
                      false
                      (recur (+ 2 i))))))))

(defn factors
  [n]
  (let [lim (inc (Math/sqrt n))]
    (loop [i 2 res []]
      (if (> i lim)
        res
        (recur (inc i)
               (if (= 0 (rem n i))
                 (conj res i (quot n i))
                 res))))))

(defn euler3
  [n]
  (->> (factors n)
       (filter prime?)
       (apply max)))

;; poly-euler.earlyprobs> (time (euler3 600851475143))
"Elapsed time: 33.237 msecs"

;; PROBLEM NO 4

(defn palin?
  [p]
  (let [st (str p)]
    (= st (apply str (reverse st)))))

(defn euler4
  [start end]
  (->> (* a b)
       (for [a (range start end)
             b (range a end)
             :when (palin? (* a b))])
       (apply max)))

;; poly-euler.earlyprobs> (time (euler4 900 1000))
"Elapsed time: 16.868 msecs"

;; PROBLEM NO 5 can be seen as finding LCM (Least common
;; multiple)

;; The first one is the most naive implementation

(defn euler5
  "Returns the smallest number that can be evely divided by all
  integers from 1 to n"
  [n]
  (loop [i (apply * (filter prime? (range 1 (inc n))))]
    (if (every? #(zero? (rem i %))
                (range 1 (inc n)))
      i
      (recur (inc i)))))

;; Elapsed time

(defn rude-lcm
  [[a & xs] res]
  (if (empty? xs)
    (conj res a)
    (if (some #(zero? (rem % a)) xs)
      (-> #(if (zero? (rem % a)) (quot % a) %)
          (map xs)
          (rude-lcm (if (prime? a) (conj res a) res)))
      (rude-lcm xs (conj res a)))))

(defn euler5a
  [n]
  (reduce *' (rude-lcm (range 1 (inc n)) [])))

;; This is a pure recursion version
;; elapsed time 0.3 msecs

(defn raw-lcm
  [n]
  (loop [[a & xs] (range 1 (inc n)) res []]
    (if (empty? xs)
      (conj res a)
      (if (some #(zero? (rem % a)) xs)
        (recur (-> #(if (zero? (rem % a)) (quot % a) %)
                   (map xs))
               (if (prime? a) (conj res a) res))
        (recur xs (conj res a))))))

(defn euler5b
  [n]
  (reduce *' (raw-lcm n)))

;; This is a Tail-recursion using loop
;; elapsed time 0.4 msecs

(defn div?
  "Returns true if a is evenly divisible by m"
  [a m]
  (zero? (rem a m)))

;; PROBLEM NO 6

(defn sum
  [ls]
  (apply +' ls))

(defn product
  [ls]
  (apply *' ls))

(defn square [x] (* x x))

(defn euler6
  [n]
  (- (sum (map square (range 1 (inc n))))
     (square (sum (range 1 (inc n))))))









