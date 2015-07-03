#lang typed/racket

(define (fibo (lim : Integer) (a : Integer) (b : Integer) (i : Integer))
  : Integer
  (if (> a lim) i (fibo lim (+ a b) a (+ i 1))))
