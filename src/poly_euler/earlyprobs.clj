(ns poly-euler.earlyprobs)

(defn euler1
  "Returns the sum of all multiplies of a or b which less than lim"
  [a b lim]
  (reduce + (filter #(or (zero? (rem % a))
                         (zero? (rem % b)))
                    (range 1 lim))))

;; poly-euler.earlyprobs> (time (euler1 3 5 1000))
;; "Elapsed time: 3.394 msecs"

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
;; "Elapsed time: 0.08507 msecs"
