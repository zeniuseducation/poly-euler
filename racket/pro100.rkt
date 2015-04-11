#lang racket

(require math)



(define (sol153 lim)
  (let ([refs (list->vector (map (λ(x) 
                                   (let ([n (remainder x 9)])
                                     (if (= n 0) 9 n)))
                                 (range (+ lim 1))))]
        [llim (+ 1 (integer-sqrt lim))])
    (define (loopi i res)
      (let ([ci (vector-ref refs i)])
        (define (loopj j)
          (if (>= j lim)
              false
              (let* ([tmp (quotient j i)]
                     [curj (vector-ref refs j)]
                     [posj (+ ci (vector-ref refs tmp))])
                (begin (if (> posj curj)
                           (vector-set! refs j posj)
                           false)
                       (loopj (+ j i))))))
        (if (>= i lim) 
            res
            (if (<= i llim)
                (begin (loopj (* i i))
                       (loopi (+ i 1) (+ res (vector-ref refs i))))
                (loopi (+ i 1) (+ res (vector-ref refs i)))))))
    (loopi 2 0)))


