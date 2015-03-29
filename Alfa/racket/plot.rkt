#lang racket

(require plot)

(define sera
  (plot (list 
         (parametric (lambda (t) 
                       (vector (* 40 t)
                               (+ 100 
                                  (- (* 30 t)
                                     (* 1/2 10 (* t t))))))
                     0 10)
         (parametric (lambda (t) 
                       (vector (* 30 t)
                               (+ 100
                                  (- (* 40 t)
                                     (* 1/2 10 (* t t))))))
                     0 10))))

(define sora
  (plot (map (lambda (x)
               (parametric 
                (lambda (t)
                  (vector (* 20 (cos (* pi (/ x 180))) t)
                          (let ((ting (+ 100 (- (* 20 (sin (* pi (/ x 180))) t)
						(* 1/2 9.81 (* t t))))))
			    (if (< ting 0) 0 ting)))) 0 6))
             (range 45 90 2))))


