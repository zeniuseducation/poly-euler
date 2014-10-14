;; This is an attempt to make some of clojure functions runs on SBCL

(defconstant true t)
(defconstant false nil)

(defun inc (x) (+ 1 x))
(defun dec (x) (- x 1))

(defun zero? (x) (zerop x))

(defun true? (x) (equal true x))

(defun false? (x) (not x))

(defun nil? (x) (null x))

(defun empty? (x) (null x))

(defun quot (a m)
  "Integer division, div"
  (floor (/ a m)))

(defun div (a m)
  "Integer division"
  (if (< a m) 0 (1+ (div (- a m) m))))

(defun filter (f ls)
  "Remove-if-not f ls"
  (remove-if-not f ls))

(defun sum (ls)
  "Returns the sum of all elements in ls"
  (apply '+ ls))

(defun product (ls)
  "Returns the product of all elements in ls"
  (apply '* ls))

(defun div? (a m)
  "Returns true if a is evenly-divisible by m"
  (zerop (rem a m)))

(defun square (x)
  (* x x))

(defun psqr? (n)
  (multiple-value-bind (x y) (truncate (sqrt n))
    (progn (evenp x)
	   (zerop y))))

(defun sqr (x) (* x x))

(defun cube (x) (* x x x))

(defun range (&rest args)
  "Clojure range behaviour"
  (cond ((= 1 (length args))
	 (let* ((i (first args)))
	   (loop for x from 0 to (1- i) collect x)))
	((= 2 (length args))
	 (let* ((i (first args)) (j (second args)))
	   (loop for x from i to (1- j) collect x)))
	((= 3 (length args))
	 (let* ((i (first args)) (j (second args)) (k (third args)))
	   (if (>= i j)
	       (loop for x from i downto (inc j) by (abs k) collect x)
	       (loop for x from i to (dec j) by k collect x))))))



(defun take (n ls)
  "Returns a list containing n first elements of ls"
  (if (= n 0)
      '()
      (if (empty? ls)
	  ls
	  (cons (first ls) (take (dec n) (rest ls))))))

(defun take-while (fn ls)
  "Returns the elements of ls starting from first while (fn elmt) is true"
  (if (empty? ls)
      ls
      (if (not (funcall fn (first ls)))
	  nil
	  (cons (first ls) (take-while fn (rest ls))))))

(defun drop (n col)
  "Drop n first elements in col"
  (if (= n 0)
      col
      (drop (dec n) (rest col))))

;; Some performance test

(defun drop-while (fn ls)
  "Returns the elements of ls starting from first while (fn elmt) is true"
  (if (empty? ls)
      ls
      (if (funcall fn (first ls))
	  (drop-while fn (rest ls))
	  ls)))



(defun every? (fn ls)
  "Returns true if every element in ls satisfies fn"
  (if (empty? ls)
      true
      (if (not (funcall fn (first ls)))
	  false
	  (every? fn (rest ls)))))

(defun some? (fn ls)
  "Returns true if at least one  element in ls satisfies fn"
  (if (empty? ls)
      false
      (if (funcall fn (first ls))
	  true
	  (every? fn (rest ls)))))

(defun partial (fn &rest args)
  "Returns a curried version of fn"
  (lambda (&rest xs) (apply fn (append args xs))))

(defun cmap-helper (fn res args)
  (if (some? 'empty? args)
      res
      (cmap-helper fn
		   (cons (apply fn (mapcar 'first args)) res)
		   (mapcar 'rest args))))

(defun cmap (fn &rest args)
  "Clojure's map behaviour"
  (if (= 1 (length args))
      (mapcar fn (first args))
      (reverse (cmap-helper fn nil args))))

(defun comp-helper (ls)
  (if (= 1 (length ls))
      (lambda (x) (funcall (first ls) x))
      (lambda (x) (funcall (comp-helper (rest ls))
		      (funcall (first ls) x)))))

(defun comp (&rest args)
  "Clojure's comp with standard clisp behaviour (you need to call it with funcall)"
  (comp-helper (reverse args)))

(defun juxt-helper (ls x)
  (if (empty? ls)
      nil
      (cons (funcall (first ls) x)
	    (juxt-helper (rest ls) x))))

(defun juxt (&rest ls)
  "Clojure's juxt with clisp behaviour"
  (lambda (x) (juxt-helper ls x)))

(defun spit (fname obj)
  "Clojure spit to file behaviour"
  (with-open-file (outfile fname
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    (prin1 obj outfile)))

(defun takelim (n ls)
  "Returns the elements in ls that less than n"
  (labels ((helper (ls res)
	     (if (> (first ls) n)
		 res
		 (helper (rest ls) (append res (list (first ls)))))))
    (helper ls nil)))

(defun group (ls)
  "Returns list of list of same elements in ls"
  (let ((sls (sort ls '<)))
    (labels ((helper (ll pres res1 res)
	       (if (empty? ll)
		   (cons pres (cons res1 res))
		   (if (= (first ll) (first pres))
		       (helper (rest ll)
			       (cons (first pres) pres)
			       res1
			       res)
		       (helper (rest ll)
			       (list (first ll))
			       pres
			       (cons res1 res))))))
      (butlast (helper (rest sls) (list (first sls)) nil nil)))))

(defun partition (ls)
  "Returns list of list of same elements in ls"
  (labels ((helper (ll pres res1 res)
	     (if (empty? ll)
		 (cons pres (cons res1 res))
		 (if (= (first ll) (first pres))
		     (helper (rest ll)
			     (cons (first pres) pres)
			     res1
			     res)
		     (helper (rest ll)
			     (list (first ll))
			     pres
			     (cons res1 res))))))
    (reverse (butlast (helper (rest ls) (list (first ls)) nil nil)))))

(defun partition-by (fn ls)
  "Returns list of list of same elements in ls"
  (labels ((helper (ll pres res1 res)
	     (if (empty? ll)
		 (cons pres (cons res1 res))
		 (if (equal  (funcall fn (first ll))
			     (funcall fn (first pres)))
		     (helper (rest ll)
			     (cons (first pres) pres)
			     res1
			     res)
		     (helper (rest ll)
			     (list (first ll))
			     pres
			     (cons res1 res))))))
    (reverse (butlast (helper (rest ls) (list (first ls)) nil nil)))))

(defun frequencies (lsm)
  (let ((sls (sort lsm '<)))
    (labels ((helper (ls p i res1 res)
	       (if (empty? ls)
		   (cons (list p i) (cons res1 res))
		   (if (= p (first ls))
		       (helper (rest ls) p (inc i) res1 res)
		       (helper (rest ls)
			       (first ls) 1
			       (list p i)
			       (cons res1 res))))))
      (butlast (helper (rest sls) (first sls) 1 nil nil)))))

(defun sort-by (fn ls)
  "Clojure sort-by"
  (sort ls '< :key fn))









