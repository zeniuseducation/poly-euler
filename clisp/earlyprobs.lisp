;; PROBLEM NO 1 

(defun range (i j)
  (loop for x from i to j
       collect x))

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

;; PROBLEM 3

(defun prime-helper (p i lim)
  (cond ((>= i lim) t)
	((zerop (rem p i)) nil)
	(t (prime-helper p (+ 2 i) lim))))

(defun prime? (p)
  (cond ((<= p 20) (if (member p '(2 3 5 7 11 13 17 19)) t nil))
	((evenp p) nil)
	(t (prime-helper p 3 (sqrt p)))))

(defun factors-helper (n i res lim)
  (cond ((> i lim)
	 res)
	((zerop (rem n i))
	 (factors-helper n (1+ i) (cons i (cons (/ n i) res)) lim))
	(t
	 (factors-helper n (1+ i) res lim))))

(defun factors (n)
  (factors-helper n 2 '() (1+ (sqrt n))))

(defun euler3 (n)
  (apply 'max
	 (remove-if-not 'prime?
			(factors n))))

;; elapsed time 37-42 msecs

(defun palin? (n)
  (let ((st (write-to-string n)))
    (equal st (reverse st))))

(defun euler4 (start end)
  (apply 'max
	 (loop for x from start to end
	    append (loop for y from x to end
		      when (palin? (* x y))
		      collect (* x y)))))

;; CL-USER> (time (euler4 900 1000))
;; Evaluation took:
;;   0.015 seconds of real time

;; PROBLEM NO 5

(defun rude-lcm (ls res) 
  (let ((a (first ls))
	(xs (rest ls)))
    (if (null xs)
	(cons a res)
	(if (some #'(lambda (x) (zerop (rem x a))) xs)
	    (rude-lcm (mapcar #'(lambda (x) (if (zerop (rem x a))
					   (/ x a)
					   x))
			      xs)
		      (if (prime? a) (cons a res) res))
	    (rude-lcm xs (cons a res))))))

(defun euler5a (n)
  (apply '* (rude-lcm (range 1 (1+ n)) nil)))


;; CL-USER> (time (euler5a 20))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000035 seconds of total run time
;; NOTES: SBCL is REALLY FAST for this kind of thing

;; Some utilities

(defun filter (f ls)
  (remove-if-not f ls))



