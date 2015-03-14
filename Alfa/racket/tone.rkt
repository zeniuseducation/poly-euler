#lang typed/racket

(define: (sum-sieve (lim : Integer))
  : Integer
  (let: ([refs : (Vectorof Boolean) (make-vector (+ 1 lim) true)])
    (define: (loopj (j : Integer)
                    (step : Integer))
      : Boolean
      (if (> j lim)
          false 
          (begin (vector-set! refs j false)
                 (loopj (+ j step) step))))
    (define: (loopi (i : Integer)
                    (res : Integer))
      : Integer
      (if (> i lim)
          res
          (if (vector-ref refs i)
              (if (<= (* i i) lim)
                  (begin (loopj (* i i) (+ i i))
                         (loopi (+ i 2) (+ res i)))
                  (loopi (+ i 2) (+ res i)))
              (loopi (+ i 2) res))))
    (loopi 3 2)))