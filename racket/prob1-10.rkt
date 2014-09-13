#lang racket 

(require "math.rkt")

;; Problem no 1

(define (euler1 a b n)
  "Returns the sum of all numbers less than n that are multiples of a or b"
  (- (+ (sum (range a n a))
        (sum (range b n b)))
     (sum (range (* a b) n (* a b)))))

;; This one took 1 msec

;; Problem no 2 

(define (even-fibo fibs res lim)
  (let* [(a (first fibs)) (b (second fibs))]
    (if (> a lim) 
        res 
        (even-fibo (cons (+ a b) fibs) 
                   (if (even? a) (+ a res) res)
                   lim))))

(define (euler2 n)
  (even-fibo (list 1 1) 0 n))

;; PROBLEM NO 3

(define (euler3 n)
  (apply max (filter prime? (factors n))))

;; This one also took less than 1 msec

(define (sum-primes n i res)
  (if (> i n)
      res
      (if (prime? i)
          (sum-primes n (+ 2 i) (+ res i))
          (sum-primes n (+ 2 i) res))))

;; Problem no 10

(define (euler10 n)
  (if (<= n 2)
      2
      (sum-primes n 3 2)))
