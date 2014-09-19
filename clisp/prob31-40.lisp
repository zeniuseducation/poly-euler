(load "math.lisp")

(defun pandig? (n)
  "Returns true if n is pandigital (containing all digits exactly once)"
  (let ((digs (numcol n)))
    (and (= 9 (length (remove-duplicates digs)))
	 (= 9 (length digs)))))

(defun find-pandig (n ls)
  (if (empty? ls)
      0
      (let* ((tmp (append (numcol n) (numcol (first ls)))))
	(if (= (length tmp) (length (remove-duplicates tmp)))
	    (let ((res1 (append tmp (numcol (/ n (first ls))))))
	      (if (= 9 (length res1) (length (remove-duplicates res1)))
		  n 
		  (find-pandig n (rest ls))))
	    (find-pandig n (rest ls))))))

(defun sol32 (start end)
  (time
   (sum (remove-duplicates
	 (mapcar #'(lambda (x) (find-pandig x (factors x)))
		 (range start end))))))

;; Problem 
