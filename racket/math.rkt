#lang racket

(provide square prime? sum product take-while cube factors)

(define (square x) (* x x))
(define (cube x) (* x x x))

(define true #t)
(define false #f)

(define (inc i) (+ 1 i))
(define (dec i) (- i 1))


(define (sum ls)
  (foldl + 0 ls))

(define (product ls)
  (foldl * 1 ls))

(define (take-while f ls)
  (takef ls f))

(define (prime-helper n i lim)
  (if (> i lim)
      true 
      (if (zero? (remainder n i))
          false 
          (prime-helper n (+ 2 i) lim))))

(define (prime? p)
  "Returns true if p is prime"
  (if (<= p 20) 
      (if (member p (list 2 3 5 7 11 13 17 19)) true false)
      (if (even? p)
          false
          (prime-helper p 3 (sqrt p)))))

(define (next-prime n)
  (cond [(= 1 n) 2]
        [(= 2 n) 3]
        [(even? n) (next-prime (inc n))]
        [(prime? (+ 2 n)) (+ 2 n)]
        [(next-prime (+ 2 n))]))

(define (prime-list-helper n i cur res)
  (if (= n i) 
      (cons cur res)
      (prime-list-helper n (inc i) (next-prime cur) (cons cur res))))

(define (prime-list n)
  "Returns the first n positive prime numbers"
  (prime-list-helper n 1 2 '()))

(define (suma-prima n)
  "Returns the sum of n first positive primes"
  (sum (prime-list n)))

(define (factors-helper n lim i res)
  (if (> i lim)
      res 
      (let [(rem (remainder n i)) (div (quotient n i))]
        (factors-helper n 
                        lim 
                        (inc i) 
                        (if (= 0 rem) 
                            (if (= i div) 
                                (cons i res)
                                (cons i (cons div res)))
                            res)))))

(define (factors n)
  "Returns the integer factors of n"
  (factors-helper n (sqrt n) 2 (list 1 n)))

(define (numcol-helper n)
  (if (< n 10)
      (list n) 
      (cons (remainder n 10) (numcol-helper (quotient n 10)))))

(define (numcol n)
  "Accepts a number n and returns a list consisting all digits of n"
  (reverse (numcol-helper n)))

(define (colnum-helper col res)
  (if (empty? col)
      res
      (colnum-helper (rest col)
                     (+ (* 10 res) (first col)))))

(define (colnum col)
  "Accepts a list of digits and convert it into number"
  (colnum-helper col 0))









