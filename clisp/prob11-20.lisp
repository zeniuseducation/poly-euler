(load "math.lisp")

;; PROBLEM NO 12
;; What is the first triangle number that has more than 500 factors

(defun triangle (n)
  "Returns the n-th triangle number"
  (/ (* n (+ 1 n)) 2))

(defun tri-factors (n)
  "Returns the number of factors of the n-th triangle number"
  (length (factors (triangle n))))

(defun solution (n i)
  (if (>= (tri-factors i) n) i (solution n (1+ i))))

(defun euler12 (n)
  "Returns the first triangle number that has n or more factors"
  (triangle (solution n 1)))

"Elapsed time 2.4 seconds!"

;; This is actually a rather naive implementation, however
;; it produces quite a fast result

;; CL-USER> (time (euler12 500))
;; Evaluation took:
;;   2.429 seconds of real time
;;   2.440775 seconds of total run time (2.431627 user, 0.009148 system)
;;   100.49% CPU
;;   3,886,596,218 processor cycles
;;   8,421,376 bytes consed     

(defun collatz (n)
  "Returns the collatz of n"
  (if (evenp n)
      (/ n 2)
      (1+ (* 3 n))))

(defun collect-collatz (n res)
  "Returns the collatz sequence starting from n"
  (if (= n 1)
      (cons 1 res)
      (collect-collatz (collatz n) (cons n res))))

(defun count-collatz (n res)
  "Returns the number of element in a collatz sequence starting from n"
  (if (= n 1)
      (+ 1 res)
      (count-collatz (collatz n) (+ 1 res))))

(defun solution14 (n)
  (let ((ls (collect-collatz n nil)))
    (list (length ls) (reverse ls))))

(defun solution14a (n)
  (count-collatz n 0))

(defun euler14-helper (i n res numb)
  "Returns the longest collatz sequence created starting from all
  integers less than n but larger than i"
  (if (= i n)
      (if (> res (solution14a n))
	  (list numb res)
	  (list i (solution14a i)))
      (let ((temp (solution14a i)))
	(if (> res temp)
	    (euler14-helper (+ 1 i) n res numb)
	    (euler14-helper (+ 1 i) n temp i)))))

(defun euler14 (i n)
  (time (euler14-helper i n 0 1)))

"Elapsed time 29-32 seconds!!"














