#lang racket

(require math)

(define (sol48 i res lim buk)
  (if (> i lim)
      res
      (sol48 (+ i 1)
	     (remainder (+ res (modular-expt i i buk)) buk)
	     lim buk)))
