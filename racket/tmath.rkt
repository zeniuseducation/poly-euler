#lang typed/racket


(define: (sqr [x : Number]) : Number (* x x))

(define: (prime? [p : Integer])
  : Boolean
  (define: (helper [i : Integer])
      : Boolean
      (if (> (* i i) p)
          true
          (if (= 0 (remainder p i))
              false
              (helper (+ 2 i)))))
    (helper 3))

(define: (sum-primes [lim : Integer])
  : Integer
  (define: (helper [i : Integer] [res : Integer])
    : Integer
    (if (> i lim)
        res
        (if (prime? i)
            (helper (+ i 2) (+ i res))
            (helper (+ i 2) res))))
  (helper 7 10))

(define: (fibo (lim : Integer))
  : Integer
  (define: (helper (i : Integer) (j : Integer) (idx : Integer))
    : Integer
    (if (> i lim)
        idx 
        (helper (+ i j) i (+ 1 idx))))
  (helper 1 1 1))
