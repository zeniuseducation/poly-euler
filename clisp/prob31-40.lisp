(load "clojure.lisp")

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

;; Problem 33

(defun conds (i j m)
  (let* ((im (colnum (list i m)))
	 (jm (colnum (list j m)))
	 (mi (colnum (list m i)))
	 (mj (colnum (list m j))))
    (or (= (/ i j) (/ im jm))
	(= (/ i j) (/ im mj))
	(= (/ i j) (/ mi mj))
	(= (/ i j) (/ mi jm)))))

(defun step33 (i j m res)
  (if (= i 10)
      res
      (if (= j 10)
	  (step33 (inc i) (inc (inc i)) 1 res)
	  (if (= m 10)
	      (step33 i (inc j) 1 res)
	      (step33 i j (inc m) (if (conds i j m)
				      (cons (list i j m) res)
				      res))))))

(defun sol33 ()
  (time (denominator
	 (apply '*
		(mapcar #'(lambda (x) (/ (first x)
				    (second x)))
			(filter #'(lambda (x) (< (first x)
					    (second x)))
				(step33 1 2 1 nil)))))))

"Elapsed time 0.8msec"

;; PRBLEM no 34

(defun fact (i)
  (if (= i 0) 1 (product (range 1 (inc i)))))

(defun cond34 (i)
  (= i (sum (mapcar 'fact (numcol i)))))

(defun sol34 (lim)
  (time (filter 'cond34 (range 10 lim))))


;; Problem 36

(defun decol-helper (n res)
  (if (< n 10)
      (cons n res)
      (decol-helper (quot n 10) (cons (rem n 10) res))))

(defun decol (n)
  (decol-helper n nil))

(defun bincol-helper (n res)
  (if (< n 2)
      (cons n res)
      (bincol-helper (quot n 2) (cons (rem n 2) res))))

(defun bincol (n)
  (bincol-helper n nil))

(defun bpalin? (n)
  (let ((decl (decol n))
	(binl (bincol n)))
    (and (equal decl (reverse decl))
	 (equal binl (reverse binl)))))

(defun sol36 (lim)
  (time (sum (filter 'bpalin? (range 1 lim)))))
