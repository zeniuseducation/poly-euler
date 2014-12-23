#lang racket 

(define (square x)
  (* x x))

(define-syntax-rule (def binding body)
  (define binding body))

(define-syntax-rule (fn binding body)
  (lambda binding body))

(def (cube x)
     (* x x x))

(def (prime? x)
     (letrec [(f (lambda (i)
		   (if (> (* i i) x)
		       true
		       (if (= 0 (remainder x i))
			   false
			   (f (+ i 2))))))]
       (f 3)))

(define (sum-primes lim)
  (letrec [(looper (lambda (i res)
		     (if (> i lim)
			 res
			 (if (prime? i)
			     (looper (+ i 2) (+ res i))
			     (looper (+ i 2) res)))))]
    (looper 3 2)))

(define (head xs)
  (car xs))

(define (tail xs)
  ((cdr xs)))

(define (lcons a xs)
  (cons a (fn () xs)))

(define (take-while f xs)
  (if (f (car xs))
      (cons (car xs) (take-while f (tail xs)))
      null))

(define (ltake n xs)
  (if (zero? n)
      null
      (cons (car xs) (ltake (- n 1) (tail xs)))))

(define lrange
  (case-lambda
    [() (lrange 0 1)]
    [(x) (lrange x 1)]
    [(x y) (cons x (fn () (lrange (+ x y) y)))]))

(define (lmap f xs)
  (cons (f (car xs))
	(fn () (lmap f (tail xs)))))

(define (iter f i)
  (cons i (fn () (iter f (f i)))))

(define lfibo
  (case-lambda
    [() (iter (fn (x)
		  (list (+ (first x) (second x))
			(first x)
			(+ 1 (third x))))
	      '[1 1 1])]
    [(i) (ltake i (lfibo))]))

(define (drop-while f xs)
  (if (f (car xs))
      (drop-while f (tail xs))
      xs))

(define (euler25 lim)
  (letrec [(looper
	    (fn (x y)
		(if (> (first x) lim)
		    y
		    (looper (list (+ (first x)
				     (second x))
				  (first x))
			    (+ y 1)))))]
    (looper '[1 1] 1)))

(define (sum-sieves lim)
  (let ((refs (make-vector lim true)))
    (define (outer i res)
      (define (inner j)
        (if (< j lim)
            (begin (vector-set! refs j false)
                   (inner (+ j (* 2 i))))
            1))
      (if (< i lim)
          (if (and (<= (* i i) lim) (vector-ref refs i))
              (begin (inner (* i i))
                     (outer (+ i 2) (+ i res)))
              (if (vector-ref refs i)
                  (outer (+ i 2) (+ i res))
                  (outer (+ i 2) res)))
          (+ 2 res)))
    (outer 3 0)))







