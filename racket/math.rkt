#lang racket

(provide square prime? sum product take-while cube factors)

(define (square x)
  (* x x))

(define true #t)
(define false #f)

(define (inc i) (+ 1 i))
(define (dec i) (- i 1))

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

(define (sum ls)
  (foldl + 0 ls))

(define (product ls)
  (foldl * 1 ls))

(define (take-while f ls)
  (takef ls f))



(define (cube x) (* x x x))










