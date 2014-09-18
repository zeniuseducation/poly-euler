(load "math.lisp")

;; Stupid attempt to diophantine equation solver


;; PROBLEM 108 AS THE BASE FOR 110

(defun inc (x) (+ 1 x))
(defun dec (x) (- x 1))

(defun diop-fy (n x res)
  "Find the first integer y in diop equation for a given n and x"
  (if (> x n)
      res
      (let ((p (- (/ 1 n) (/ 1 x))))
	(if (= 1 (numerator p))
	    (diop-fy n (inc x) (1+ res))
	    (diop-fy n (inc x) res)))))

(defun sol108 (target start)
  (if (<= target (diop-fy start 1 0))
      start
      (sol108 target (+ 1 start))))


