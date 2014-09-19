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
