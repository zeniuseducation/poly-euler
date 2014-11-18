#lang racket

(define (prime? p)
  (define (helper i lim)
    (if (> i lim) 
        true
        (if (= 0 (remainder p i))
            false
            (helper (+ 2 i) lim))))
  (helper 3 (sqrt p)))

(define (suma-prima lim)
  (define (helper i res)
    (if (> i lim )
        res
        (if (prime? i)
            (helper (+ 2 i) (+ res i))
            (helper (+ 2 i) res))))
  (helper 7 10))

(define (fibo-sol i j idx lim)
  (if (> i lim)
      idx
      (fibo-sol (+ i j) i (+ 1 idx) lim)))

