#lang racket

(require math)
(define rem remainder)
(define div quotient)
(define neg? negative?)
(define rd radians->degrees)
(define dr degrees->radians)

(define (gdeg m)
  (rd (atan m)))

(define (grad point)
  (/ (* -4 (first point)) (second point)))

(define l list)

(define (lgrad p1 p2)
  (match-let ([(list x1 y1) p1]
              [(list x2 y2) p2])
    (let ((m (/ (- y2 y1) (- x2 x1)))) m)))

(define (next-grad m1 m2)
  (let* ((dm1 (gdeg m1))
         (dm2 (gdeg m2))
         (diff (- dm1 dm2)))
    (tan (dr (- dm2 diff)))))

(define (f= x i)
  (<= (- i 0.01) x (+ i 0.01)))

(define (solve-line p m c)
  (match-let* (((list x y) p)
               (det (sqrt (- (* m m) (* 16 (- c 100)))))
               (x1 (/ (+ (- m) det) 8))
               (x2 (/ (- (- m) det) 8)))
    (if (f= x x1) 
        (list x2 (which-one x x2))
        (list x1 (which-one x x1)))))
    
    
(define (something else)
  else)

(define (next-impact p1 p2)
  (match-let* (((list x1 y1) p1)
               ((list x2 y2) p2)
               (m (lgrad p1 p2)))
    (solve-line p2 m (- y2 (* m x2)))))
    
    

(define (sol144 p1 p2 (res 0))
  (match-let* (((list x1 y1) p1)
               ((list x2 y2) p2))
    (cond ((f= y1 10) (if (f= x 0) res (sol144 p2 (next-impact p1 p2) (inc res))))
          (#t (sol144 p2 (next-impact p1 p2) (inc res))))))


(define (fibo n)
  (cond ((= n 1) (begin (display n) 1))
        ((= n 2) (begin (display n) 2))
        (#t (begin (display n)
                   (+ (fibo (- n 1))
                      (fibo (- n 2)))))))

(define (nfibo i)
  (cond ((= i 1) 1)
        ((= i 2) 2)
        (#t (+ (nfibo (- i 1))
               (nfibo (- i 2))))))

(define (ifibo i (a 2) (b 1) (idx 2))
  (cond ((= i 1) 1)
        ((= idx i) a)
        (#t (ifibo i (+ a b) a (+ idx 1)))))

(define (lfibo i)
  (define (loopi idx res)
    (if (= i idx)
        (reverse res)
        (loopi (+ idx 1) (cons (+ (first res)
                                  (second res))
                               res))))
  (loopi 2 (list 2 1)))

(define (sum-sieve lim)
  (let* ((llim (ceiling (sqrt lim)))
         (primes (make-vector (+ lim 1) true)))
    (define (outer i res)
      (if (> i lim) 
          res 
          (if (vector-ref primes i)
              (begin (when (<= i llim)
                       (define (inner j)
                         (when (<= j lim)
                           (vector-set! primes j false)
                           (inner (+ j i i))))
                       (inner (* i i)))
                     (outer (+ i 2) (+ res i)))
              (outer (+ i 2) res))))
    (outer 3 2)))

(define (sum-tots lim)
  (let* ((llim (ceiling (sqrt lim)))
         (primes (make-vector (+ lim 1) true))
         (tots (list->vector (range (+ lim 1)))))
    (define (outer i res)
      (if (> i lim)
          res
          (if (vector-ref primes i)
              (begin (when (<= i llim)
                       (define (inner j)
                         (when (<= j lim)
                           (vector-set! primes j false)
                           (inner (+ j i i))))
                       (define (itots j)
                         (when (<= j lim)
                           (vector-set! tots 
                                        j 
                                        (div (* (- i 1)
                                                (vector-ref tots j)) i))
                           (itots (+ j i i))))
                       (inner (* i i))
                       (itots (* 3 i)))
                     (outer (+ i 2) (+ res (- i 1))))
              (outer (+ i 2) (+ res (vector-ref tots i))))))
    (outer 3 1)))

(define (sum-tots2 lim)
  (let* ((llim (ceiling (sqrt lim)))
         (primes (make-vector (+ lim 1) true))
         (tots (list->vector (range (+ lim 1)))))
    (define (inner j i)
      (when (<= j lim)
        (vector-set! primes j false)
        (inner (+ j i i) i)))
    (define (itots j i)
      (when (<= j lim)
        (vector-set! tots 
                     j 
                     (div (* (- i 1)
                             (vector-ref tots j)) i))
        (itots (+ j i i) i)))
    (define (outer i res)
      (if (> i lim)
          res
          (if (vector-ref primes i)
              (begin (when (<= i llim)      
                       (inner (* i i) i)
                       (itots (* 3 i) i))
                     (outer (+ i 2) (+ res (- i 1))))
              (outer (+ i 2) (+ res (vector-ref tots i))))))
    (outer 3 1)))

(define (sum-or n a b)
  (let ((na (div (- n 1) a))
        (nb (div (- n 1) b))
        (nab (div (- n 1) (* a b))))
    (- (+ (div (* na (+ a (* na a))) 2)
          (div (* nb (+ b (* nb b))) 2))
       (div (* nab (+ (* a b) (* nab (* a b)))) 2))))

(define (big-sqr lim)
  (define (iter i)
    (let ((isqr (* i i)))
      (if (> (- isqr i) lim)
          i
          (iter (+ i 1)))))
  (iter 1))

(define (inc i) (+ 1 i))
(define (dec i) (- i 1))

(define (sol407 lim)
  (let* ((nref (make-vector (+ lim 1) 0)))
    (define (fill-n a xs)
      (when (not (empty? xs))
        (let* ((x (first xs))
               (rxs (rest xs)))
          (if (<= x a)
              (fill-n a rxs)
              (when (<= x lim)
                (let ((nr (vector-ref nref x)))
                  (when (> a nr)
                    (vector-set! nref x a)
                    (fill-n a rxs))))))))
    (define (iter a)
      (when (< a lim)
        (let* ((asd (- (* a a) a)))
          (fill-n a (divisors asd)))
        (iter (+ a 1))))
    (iter 1)
    (foldl + 0 (vector->list nref))))

(define (next-col n)
  (cond ((= 0 (rem n 3)) (l "D" (div n 3)))
        ((= 1 (rem n 3)) (l "U" (div (+ 2 (* 4 n)) 3)))
        ((= 2 (rem n 3)) (l "d" (div (- (* 2 n) 1) 3)))))

(define (collatz n (ast ""))
  (if (= n 1)
      ast
      (match-let* (((list st num) (next-col n)))
        (collatz num (string-append ast st)))))

(define (colat n st)
  (cond ((= 0 (string-length st)) true)
        ((= n 1) false)
        (#t (match-let* (((list sts num) (next-col n)))
              (if (string=? sts (substring st 0 1))
                  (colat num (substring st 1))
                  false)))))

(define (starter lim)
  (define (iter-1 i)
    (if (= 0 (rem i 3)) i (iter-1 (+ 1 i))))
  (define (iter-2 i)
    (let ((n (+ 6 (* 4 i))))
      (if (= 0 (rem n 81)) n (iter-2 (+ i 3)))))
  (iter-2 (iter-1 lim)))

(define (next-stp i)
  (define (iter n)
    (let* ((sp1 (- n 2)))
      (if (= 0 (rem sp1 4))
          (let* ((sp2 (div sp1 4)))
            (if (= 1 (rem sp2 3))
                sp2
                (iter (+ n 81))))
          (iter (+ n 81)))))
  (iter i))

(define (sol277 start st)
  (define (iter i)
    (if (colat i st)
        i 
        (iter (+ 81 i))))
  (iter start))

(define (revcol i st)
  (if (string=? st "")
      i
      (let* ((sl (string-length st))
             (chr (substring st (dec sl) sl))
             (sres (substring st 0 (dec sl))))
        (cond ((string=? "D" chr) (revcol (* 3 i) sres))
              ((string=? "U" chr) (revcol (div (- (* 3 i) 2) 4) sres))
              ((string=? "d" chr) (revcol (div (+ 1 (* 3 i)) 2) sres))))))
            





















