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
