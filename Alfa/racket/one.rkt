#lang racket

(require math)

(define (fibo a b i lim)
  (if (> a lim)
      i
      (fibo (+ a b) a (+ 1 i) lim)))

(define (sum-primes n (i 3) (res 2))
  (if (> i n)
      res 
      (sum-primes n (+ i 2) 
                  (if (prime? i) 
                      (+ res i)
                      res))))

(define (sum-sieve lim)
  (let ([refs (make-vector (+ lim 1) true)]
        [llim (sqrt lim)])
    (define (loopj j step)
      (if (> j lim)
          null
          (begin (vector-set! refs j false)
                 (loopj (+ j step) step))))
    (define (loopi i res)
      (if (> i lim)
          res
          (if (vector-ref refs i)
              (if (<= i llim)
                  (begin (loopj (* i i) (+ i i))
                         (loopi (+ i 2) (+ res i)))
                  (loopi (+ i 2) (+ res i)))
              (loopi (+ i 2) res))))
    (loopi 3 2)))

(define (log10 a)
  (/ (log a) (log 10)))

(define (repeat-d lim)
  (define (check x (diva 1000) (res 0) (xs '[]))
    (let ([t1 (remainder diva x)])
      (if (member t1 xs)
	  res
	  (check x (* 10 t1) (+ 1 res) (cons t1 xs)))))
  (define (loopi i res)
    (if (< i (second res))
	(first res)
	(let ([tmp (check i)])
	  (if (> tmp (second res))
	      (loopi (prev-prime i) (list i tmp))
	      (loopi (prev-prime i) res)))))
  (if (prime? lim)
      (loopi lim (list lim 0))
      (repeat-d (prev-prime lim))))


