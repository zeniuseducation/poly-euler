;; PROBLEM NO 1 

(defun range (i j)
  (if (= i j)
      nil
      (cons i (range (1+ i) j))))

(defun euler1 (a b lim)
  (reduce '+ (remove-if-not #'(lambda (x) (or (zerop (rem x a))
					 (zerop (rem x b))))
			    (range 1 lim))))

;; CL-USER> (time (euler1 3 5 1000))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000281 seconds of total run time
;;   100.00% CPU
;;   443,738 processor cycles
;;   32,768 bytes consed

(defun euler2 (ls res lim)
  "Returns the sum of all even valued elements of fibo numbers less
  than lim"
  (if (>= (first ls) lim)
      res
      (euler2 (list (+ (first ls)
		       (second ls))
		    (first ls))
	      (if (evenp (first ls))
		  (+ (first ls) res)
		  res)
	      lim)))
