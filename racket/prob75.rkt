#lang racket

(require "math.rkt")

(define (sorted-factors n)
  "returns the sorted factors of n starting from the smallest"
  (sort (factors n) <))

(define (psqr? x)
  (let ((xsqrt (sqrt x)))
    (= xsqrt (truncate xsqrt))))

(define (pita lim)
  "Returns all possible combination of pitagorean triplets with peripheral at most lim"
  (for*/list ([a (in-range 3 (/ lim 3))])
    (let ((asqr (sqr a)))
      (for*/list ([b (in-range a (/ lim 2))]
                  #:when (and (psqr? (+ asqr (sqr b)))
                              (<= (+ a b (sqrt (+ asqr (sqr b)))) lim))
                  #:break (> (- (sqr (inc b)) (sqr b)) asqr))
        (+ a b (round (sqrt (+ asqr (sqr b)))))))))

(define (pitas lim)
  (time (apply append (pita lim))))






