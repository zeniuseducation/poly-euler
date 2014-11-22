#lang typed/racket

(define: (sqr [x : Number]) : Number (* x x))

(define: (prime? [p : Integer])
  : Boolean
  (define: (helper [i : Integer])
      : Boolean
      (if (> (* i i) p)
          true
          (if (= 0 (remainder p i))
              false
              (helper (+ 2 i)))))
    (helper 3))

(: true-prime? (-> Integer Boolean))
(define (true-prime? p)
  (cond ((= 2 p) true)
        ((even? p) false)
        (true (define: (helper (i : Integer))
                : Boolean
                (if (> (* i i) p)
                    true
                    (if (= 0 (remainder p i))
                        false
                        (helper (+ i 2)))))
              (helper 3))))

(define: (next-prime (i : Integer))
  : Integer
  (if (= i 2)
      3
      (if (prime? (+ i 2))
          (+ i 2)
          (next-prime (+ i 2)))))

(define: (nth-prime (i : Integer))
  : Integer
  (define: (helper (j : Integer) (res : Integer))
    : Integer
    (if (= j i) 
        res
        (helper (+ 1 j) (next-prime res))))
  (helper 1 2))

(define: (sum-primes [lim : Integer])
  : Integer
  (define: (helper [i : Integer] [res : Integer])
    : Integer
    (if (> i lim)
        res
        (if (prime? i)
            (helper (+ i 2) (+ i res))
            (helper (+ i 2) res))))
  (helper 7 10))

(define: limits : Integer (expt 10 999))

(define: (fibo (lim : Integer))
  : Integer
  (define: (helper (i : Integer) (j : Integer) (idx : Integer))
    : Integer
    (if (> i lim)
        idx 
        (helper (+ i j) i (+ 1 idx))))
  (helper 1 1 1))

(define: (pfactors (n : Integer))
  : (Listof Integer)
  (define: (helper (i : Integer)
                   (p : Integer) 
                   (lasti : Integer)
                   (res : (Listof Integer)))
    : (Listof Integer)
    (if (true-prime? p)
        (if (= p lasti)
            res
            (cons p res))
        (let: ((rems : Integer (remainder p i))
               (divs : Integer (quotient p i)))
          (if (= 0 rems)
              (helper 2 divs i (cons i res))
              (helper (next-prime i) p lasti res)))))
  (helper 2 n 2 (list)))

(define: (largest-pfactors (n : Integer))
  : Integer
  (define: (helper (i : Integer)
                   (p : Integer) 
                   (stat : Boolean))
    : Integer
    (if stat
        (if (prime? p)
            p
            (helper i p false))
        (if (= 0 (remainder p i))
            (helper 2 (quotient p i) true)
            (helper (next-prime i) p false))))
  (helper 2 n true))

(define: (count-factors (n : Integer))
  : Integer
    (define: (helper-odd (i : Integer) (res : Integer))
      : Integer
      (if (> (* i i) n)
          res
          (if (= 0 (remainder n i))
              (let: ((divs : Integer (quotient n i)))
                (if (= i divs)
                    (+ 1 res)
                    (helper-odd (+ i 2) (+ 2 res))))
              (helper-odd (+ i 2) res))))
    (define: (helper-even (i : Integer) (res : Integer))
      : Integer
      (if (> (* i i) n)
          res
          (if (= 0 (remainder n i))
              (let: ((divs : Integer (quotient n i)))
                (if (= i divs)
                    (+ 1 res)
                    (helper-even (+ i 1) (+ 2 res))))
              (helper-even (+ i 1) res))))
    (if (even? n)
        (helper-even 2 2)
        (helper-odd 3 2)))

(define: (first-triangle-having-lim-factors (n : Integer) (lim : Integer))
  : (Listof Integer)
  (let: ((triangle : Integer (quotient (* n (+ n 1)) 2)))
    (if (> (count-factors triangle) lim)
        (list n triangle)
        (first-triangle-having-lim-factors (+ 1 n) lim))))

(define: (collatz (n : Integer))
  : Integer
  (if (= n 1)
      1
      (+ 1 (if (even? n)
               (collatz (quotient n 2))
               (collatz (+ 1 (* 3 n)))))))

(define: (max-collatz (start : Integer) (lim : Integer))
  : Integer
  (define: (helper (i : Integer)
                   (res : Integer)
                   (lres : Integer))
    : Integer
    (if (> i lim)
        res
        (let: ((collz : Integer (collatz i)))
          (if (> collz lres)
              (helper (+ 2 i) i collz)
              (helper (+ 2 i) res lres)))))
  (helper start 1 1))

(define: (sum-pdivs (n : Integer))
  : Integer
    (define: (helper-odd (i : Integer) (res : Integer))
      : Integer
      (if (> (* i i) n)
          res
          (if (= 0 (remainder n i))
              (let: ((divs : Integer (quotient n i)))
                (if (= i divs)
                    (+ i res)
                    (helper-odd (+ i 2) (+ i divs res))))
              (helper-odd (+ i 2) res))))
    (define: (helper-even (i : Integer) (res : Integer))
      : Integer
      (if (> (* i i) n)
          res
          (if (= 0 (remainder n i))
              (let: ((divs : Integer (quotient n i)))
                (if (= i divs)
                    (+ i res)
                    (helper-even (+ i 1) (+ i divs res))))
              (helper-even (+ i 1) res))))
    (if (even? n)
        (helper-even 2 1)
        (helper-odd 3 1)))

(define: (sum-amic (lim : Integer))
  : Integer
  (define: (helper (i : Integer) (res : Integer))
    : Integer
    (if (> i lim)
        res
        (let: ((amic : Integer (sum-pdivs i)))
          (if (= i amic)
              (helper (+ 1 i) res)
              (let: ((div-amic : Integer (sum-pdivs amic)))
                (if (= i div-amic)
                    (helper (+ 1 i) (+ i res))
                    (helper (+ 1 i) res)))))))
  (helper 2 0))

(define: (sum-amicables (lim : Integer))
  : Integer
  (define: (helper (i : Integer) (res : Integer) (refs : (Listof Integer)))
    : Integer
    (if (> i lim)
        res
        (if (member i refs)
            (helper (+ i 1) res refs)
            (let: ((amic : Integer (sum-pdivs i)))
              (if (= i amic)
                  (helper (+ 1 i) res refs)
                  (let: ((div-amic : Integer (sum-pdivs amic)))
                    (if (= i div-amic)
                        (helper (+ 1 i) (+ i amic res) (cons amic refs))
                        (helper (+ 1 i) res refs))))))))
  (helper 2 0 '()))

(define: (fill-vectors (lim : Integer))
  : (Vectorof Integer)
  (let: ((the-vec : (Vectorof Integer) (make-vector lim 0)))
    (define: (helper (i : Integer))
      : (Vectorof Integer)
      (if (< i lim)
          (begin (vector-set! the-vec i i)
                 (helper (+ 1 i)))
          the-vec))
    (helper 1)))

(define: (sum-sieves (lim : Integer))
  : Integer
  (let: ((refs : (Vectorof Boolean) (make-vector lim true)))
    (define: (outer (i : Integer) (res : Integer))
      : Integer
      (define: (inner (j : Integer))
        : Integer
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

(define: (nth-sieves (m : Integer) (n : Integer))
  : Integer
  (let*: ((lim : Integer (* m n))
          (refs : (Vectorof Boolean) (make-vector lim true)))
    (define: (outer (i : Integer) (p : Integer) (res : Integer))
      : Integer
      (define: (inner (j : Integer))
        : Integer
        (if (< j lim)
            (begin (vector-set! refs j false)
                   (inner (+ j (* 2 i))))
            1))
      (if (< p m)
          (if (and (<= (* i i) lim) (vector-ref refs i))
              (begin (inner (* i i))
                     (outer (+ i 2) (+ p 1) i))
              (if (vector-ref refs i)
                  (outer (+ i 2) (+ p 1) i)
                  (outer (+ i 2) p res)))
          res))
    (outer 3 1 2)))

(define: (abundants (lim : Integer))
  : (Listof Integer)
  (define: (helper (i : Integer) (res : (Listof Integer)))
    : (Listof Integer)
    (if (= i 0)
        res
        (if (< i (sum-pdivs i))
            (helper (- i 1) (cons i res))
            (helper (- i 1) res))))
  (helper lim '()))

(define: (non-abundant-sum (lim : Integer))
  : Integer 
  (let: ((refs : (Vectorof Boolean) (make-vector lim true))
         (main-list : (Listof Integer) (abundants lim)))
    (define: (helper (ls : (Listof Integer)))
      : Integer 
      (define: (ihelper (x : Integer) (xs : (Listof Integer)))
        : Integer
        (let: ((idx : Integer (+ x (first xs))))
          (if (>= idx lim)
              0
              (begin (vector-set! refs idx false)
                     (ihelper x (rest xs))))))
      (define: (sum-helper (i : Integer) (res : Integer))
        : Integer
        (if (>= i lim)
            res
            (if (vector-ref refs i)
                (sum-helper (+ i 1) (+ i res))
                (sum-helper (+ i 1) res))))
      (if (empty? ls)
          (sum-helper 1 0)
          (begin (ihelper (first ls) ls)
                 (helper (rest ls)))))
    (helper main-list)))

(define: (pita (lim : Integer))
  : Integer
  (define: (outer (a : Integer))
    : Integer 
    (define: (inner (b : Integer))
      : Integer 
      (let: ((c : Integer (- lim b a)))
        (if (> b c)
            0
            (if (= (+ (* a a) (* b b))
                   (* c c))
                (* a b c)
                (inner (+ 1 b))))))
    (let: ((res : Integer (inner (+ a 1))))
      (if (= 0 res) (outer (+ a 1)) res)))
  (outer 3))

(define: (find-cycle (n : Integer))
  : Integer
  (let ((refs : (Vectorof Boolean) (make-vector (+ 2 n) false))
        (refs2 : (Vectorof Boolean) (make-vector (+ 2 n) false)))
    (define: (iter (i : Integer) (res : Integer) (res2 : Integer))
      : Integer
      (if (vector-ref refs2 i)
          res2
          (let ((rems (remainder (* 10 i) n)))
            (if (= 0 rems)
                0
                (if (vector-ref refs i)
                    (begin (vector-set! refs2 i true)
                           (iter rems res (+ 1 res2)))
                    (begin (vector-set! refs i true)
                           (iter rems (+ 1 res) res2)))))))
    (iter 1 0 0)))

(define: (max-cycle (lim : Integer))
  : (Listof Integer)
  (define: (iter (i : Integer) (n : Integer) (res : Integer))
    : (Listof Integer)
    (if (> res i)
        (list n res)
        (let ((tmp (find-cycle i)))
          (if (> tmp res)
              (iter (- i 1) i tmp)
              (iter (- i 1) n res)))))
  (iter lim lim 0))

























































