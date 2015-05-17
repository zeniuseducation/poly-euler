#lang racket

(define/match (reduce f xs (acc null))
  ([f (list-rest x dxs) (? null?)] (reduce f dxs x))
  ([_ (? empty?) acc] acc)
  ([f (list-rest x dxs) acc] (reduce f dxs (f acc x))))

(define (sum xs) (reduce + xs))

(define (prod xs) (reduce * xs))

(define/match (take-while f xs (acc '[]))
  ([_ (? empty?) acc] (reverse acc))
  ([f (list-rest x dxs) acc]
   (if (f x) 
       (take-while f dxs (cons x acc))
       (reverse acc))))

(define/match (drop-while f xs)
  ([_ (? empty?)] xs)
  ([f (list-rest x dxs)] (if (f x) (drop-while f dxs) xs)))

;; Problem 1

(define (sol1 lim)
  (- (+ (reduce + (range 3 lim 3))
        (reduce + (range 5 lim 5)))
     (reduce + (range 15 lim 15))))

;; Problem 2

(define (sol2 lim (a 1) (b 0) (res 0)) 
  (cond [(> a lim) res]
        [(even? a) (sol2 lim (+ a b) a (+ res a))]
        [else (sol2 lim (+ a b) a res)]))

(define/match (prime? p)
  ([2] true)
  ([(? (lambda (x) (< x 2)))] false)
  ([(? even?)] false)
  ([else] (let [(lim (sqrt p))]
            (define (iter i)
              (cond [(> i lim) true]
                    [(= 0 (remainder p i)) false]
                    [else (iter (+ i 2))]))
            (iter 3))))

(define (odd-prime? p)
  (let [(lim (sqrt p))]
    (define (iter i)
      (cond [(> i lim) true]
            [(= 0 (remainder p i)) false]
            [else (iter (+ i 2))]))
    (iter 3)))

;; Problem 3 -> works only for odd n

(define (sol3 n (p 3))
  (cond [(= 1 n) (- p 2)]
        [(odd-prime? p)
         (if (= 0 (remainder n p))
             (letrec [(g (lambda (x)
                           (if (= 0 (remainder x p))
                               (g (quotient x p))
                               x)))]
               (sol3 (g n) (+ p 2)))
             (sol3 n (+ p 2)))]
        [else (sol3 n (+ p 2))]))

(define (numcol n (acc '[]))
  (if (< n 10)
      (cons n acc)
      (numcol (quotient n 10)
              (cons (remainder n 10) acc))))

(define/match (colnum xs (res 0))
  ([(? empty?) res] res)
  ([(list-rest x dxs) res] (colnum dxs (+ (* res 10) x))))

(define (palindrome? xs)
  (equal? xs (reverse xs)))

;; Problem 4

(define (sol4-1 lim)
  (apply max
         (for*/list [(i (range 900 lim))
                     (j (range 900 lim))
                     #:when (and (not (= i j))
                                 (palindrome? (numcol (* i j))))]
           (* i j))))


;; Problem 7 naive

(define (sol7-1 target (p 3) (i 1))
  (if (odd-prime? p)
      (if (= i target) p (sol7-1 target (+ p 2) (+ i 2)))
      (sol7-1 target (+ p 2) i)))

;; Problem 7 using sieve with size input as the limit for sieving
;; lim = target * size, if size is not supplied then default to 11

(define (sol7-2 target (size 11))
  (let* [(lim (* size target))
         (llim (sqrt lim))
         (refs (make-vector (+ lim 1) true))]
    (define (iter-1 i (ctr 1))
      (define (iter-2 j)
        (if (> j lim)
            false
            (begin (vector-set! refs j false)
                   (iter-2 (+ j i i)))))
      (cond [(> i lim) "Fail to find the result"]
            [(vector-ref refs i)
             (cond [(= ctr (- target 1)) i]
                   [(<= i llim) 
                    (begin (iter-2 (* i i))
                           (iter-1 (+ 2 i) (+ ctr 1)))]
                   [else (iter-1 (+ i 2) (+ ctr 1))])]
            [else (iter-1 (+ i 2) ctr)]))
    (iter-1 3)))


;; Problem 10 naive

(define (sum-primes lim (i 3) (res 2))
  (cond [(>= i lim) res]
        [(odd-prime? i) (sum-primes lim (+ i 2) (+ res i))]
        [else (sum-primes lim (+ i 2) res)]))

(define (suma-prima lim)
  (define (iter i res)
    (cond [(>= i lim) res]
          [(odd-prime? i) (iter (+ i 2) (+ i res))]
          [else (iter (+ i 2) res)]))
  (iter 3 2))

;; Typical solution problem 10 using sieve

(define (sum-sieve lim)
  "Summing all primes less than lim"
  (let [(llim (sqrt lim))
        (refs (make-vector (+ lim 1) true))]
    (define (iter-1 i (res 2))
      "Main iteration of i searching for primes and accumulate the result"
      (define (iter-2 j)
        "This one is the inner iteration to sieve the multiples of i"
        (if (> j lim) 
            false 
            (begin (vector-set! refs j false)
                   (iter-2 (+ j i i)))))
      (cond [(> i lim) res]
            [(vector-ref refs i) 
             (if (<= i llim)
                 (begin (iter-2 (* i i))
                        (iter-1 (+ i 2) (+ i res)))
                 (iter-1 (+ i 2) (+ i res)))]
            [else (iter-1 (+ i 2) res)]))
    (iter-1 3)))

(define (sieve lim)
  (let [(refs (make-vector (+ lim 1) true))
        (llim (sqrt lim))
        (res 2)]
    (for [(i (range 3 (+ llim 1)))
           #:when (vector-ref refs i)]
      (for [(j (range (* i i) lim (* 2 i)))]
        (vector-set! refs j false))
      (set! res (+ i res)))
    res))

(define (sum-sieve-2 lim)
  (let [(refs (make-vector (+ lim 1) true))
        (llim (integer-sqrt lim))
        (res 2)]
    (for [(i (range 3 (+ llim 1) 2))
          #:when (vector-ref refs i)]
      (for [(j (range (* i i) (+ lim 1) (+ i i)))]
        (vector-set! refs j false))
      (set! res (+ res i)))
    (for [(i (range (if (even? llim)
                        (+ llim 1)
                        llim) (+ lim 1) 2))
          #:when (vector-ref refs i)]
      (set! res (+ res i)))
    res))



;; Problem 25

(define lim (expt 10 999))

(define (sol25 lim (a 1) (b 0) (i 1))
  (if (> a lim) i (sol25 lim (+ a b) a (+ i 1))))
