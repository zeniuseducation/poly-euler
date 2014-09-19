;; PROBLEM NO 25

;; Suppose F(n) is the n-th element of fibonacci sequence starting
;; from 1,1. Find n in which F(n) is the first element to reach 1000 digits.

(defun euler25 (lim ls i)
  "Returns the i-th element of fibonacci that first reach 1000 digits"
  (if (>= (first ls) lim)
      i
      (euler25 lim
	       (list (+ (first ls)
			(second ls))
		     (first ls))
	       (1+ i))))

;; CL-USER> (time (euler25 (expt 10 999) '(1 1) 2))
;; Evaluation took:
;;   0.001 seconds of real time
;;   0.001455 seconds of total run time (0.001441 user, 0.000014 system)
;;   100.00% CPU
;;   2,321,798 processor cycles
;;   1,239,936 bytes consed


;; PROBLEM 21
;; Sum of all amicable numbers less than 10000

(defun sum-divisors (n)
  (sum (filter #'(lambda (x) (= 0 (rem n x))) (range 1 (inc (quot n 2))))))

(defun amics (a)
  (let ((pair (sum-factors a)))
    (if (and (/= a pair) (= a (sum-factors pair))) a nil)))

(defun sol21 (lim)
  (time (sum (filter 'amics (range 2 lim)))))

"Elapsed time 0.052 seconds!"

;; Problem 24

(defun step24 (n dig res raw)
  (if (= 0 dig)
      res
      (let ((fak (product (range 1 dig))))
	(step24 (rem n fak)
		(dec dig)
		(cons (nth (quot n fak) raw) res)
		(remove (nth (quot n fak) raw) raw)))))

(defun sol24 (n)
  (time (colnum (reverse (step24 n 10 nil (range 10))))))

"Elapsed time 0.000038 seconds!!, input value n = 999999"

;; Problem 29

(defun sol29 (n)
  (time (length (remove-duplicates
		 (loop for i from 2 to n
		    append (loop for j from 2 to n
			      collect (expt i j)))))))

"elapsed time 0.019 sec"

;; Problem 30

(defun sol30 (n)
  (time (sum (loop for i from 2 to (* n (expt 9 n))
		when (= i (sum (mapcar #'(lambda (x) (expt x n))
				       (numcol i))))
		collect i))))



