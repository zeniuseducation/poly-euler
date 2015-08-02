#lang racket

(require math)
(define rem remainder)
(define div quotient)
(define neg? negative?)

(define (sol5 lim)
  (let [(faks (apply vector (range (+ lim 2))))
        (res 1)]
    (define (outer i)
      (let [(p (vector-ref faks i))]
        (define (inner j)
          (when (<= j lim)
            (vector-set! faks j (div (vector-ref faks j) p))
            (inner (+ j i))))
        (if (> i lim)
            res
            (begin (inner (* i 2))
                   (set! res (* res p))
                   (outer (+ i 1))))))
    (outer 2)))

(define (sol7 nth)
  (let* [(lim (* 12 nth))
         (llim (integer-sqrt lim))
         (hlim (if (even? llim) (+ llim 1) (+ 2 llim)))
         (primes (make-vector lim true))]
    (define (outer i cur)
      (define (inner j)
        (when (<= j lim)
          (vector-set! primes j false)
          (inner (+ j i i))))
      (if (> i llim)
          cur
          (if (vector-ref primes i)
              (begin (inner (* i i))
                     (outer (+ i 2) (+ cur 1)))
              (outer (+ i 2) cur))))
    (define (finder i cur)
      (if (= cur nth)
          (- i 2)
          (if (vector-ref primes i)
              (finder (+ i 2) (+ 1 cur))
              (finder (+ i 2) cur))))
    (finder hlim (outer 3 1))))



