#lang racket

(define (square x)
  (* x x))

(define true #t)
(define false #f)

(define (prime-helper n i lim)
  (if (> i lim)
      true 
      (if (zero? (remainder n i))
          false 
          (prime-helper n (+ 2 i) lim))))

(define (prime? p)
  (if (<= p 20) 
      (if (member p (list 2 3 5 7 11 13 17 19)) true false)
      (if (even? p)
          false
          (prime-helper p 3 (sqrt p)))))

(define (sum ls)
  (foldl + 0 ls))

(define (product ls)
  (foldl * 1 ls))

(define (take-while f ls)
  (takef ls f))

(define (sum-primes n i res)
  (if (> i n)
      res
      (if (prime? i)
          (sum-primes n (+ 2 i) (+ res i))
          (sum-primes n (+ 2 i) res))))

(define (euler10 n)
  (if (<= n 2)
      2
      (sum-primes n 3 2)))

(define (cube x) (* x x x))










