#lang typed/racket

(require math)

(define: (sol179 (lim : Integer))
  : Integer
  (define: (loop (i : Integer) (res : Integer))
    : Integer
    (if (> i lim)
        res 
        (if (= (length (divisors i))
               (length (divisors (+ i 1))))
            (loop (+ i 1) (+ res 1))
            (loop (+ i 1) res))))
  (loop 10 0))