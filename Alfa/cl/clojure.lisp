;; This is an attempt to make some of clojure functions runs on SBCL

;; Basic rules: if the name of a macro/function does not exist in
;; standard CL then it will be used. If such symbol already used in CL
;; vocab, then we'll use a prefix c... for example:

;; range doesnt exist in CL therefore we use range however let already
;; exists in CL therefore we use clet. if you're not sure, then you
;; can simply use c prefix for all clojure built-in macros and
;; functions, and you're good to go.


(defconstant true t)
(defconstant false nil)

(defun clojure-vector-char (stream char)
  `',(read-delimited-list #\] stream t))

(set-macro-character #\[ #'clojure-vector-char)
(set-macro-character #\] (get-macro-character #\)))

(defmacro deff (&rest ftypes)
  (if (null ftypes)
      `(list `declare `(optimize (speed 3)))
      `(list `declare `(optimize (speed 3))
	     (cons `fixnum (list ,@ftypes)))))

(defmacro def (vname vbody)
  `(defparameter ,vname ,vbody))

(defmacro fn (body)
  `(lambda (%) ,body))

(defmacro fn2 (body)
  `(lambda (%1 %2) ,body))

(defmacro fn3 (body)
  `(lambda (%1 %2 %3) ,body))

(defmacro cmap (f &rest lst)
  `(mapcar ,f ,@lst))

(defmacro ->> (&body body)
  (reduce (fn2 (append %2 (list %1))) body))

(defmacro -> (&body body)
  (reduce (fn2 (append (list (first %2))
		       (list %1)
		       (rest %2)))
	  body))

(defun take-odd (xs)
  (deff)
  (labels ((looper (ls res)
	      (deff)
	      (if (null ls)
		  res
		  (looper (rest (rest ls))
		     (cons (first ls) res)))))
    (looper (rest (reverse xs)) nil)))

(defun take-even (xs)
  (deff)
  (labels ((looper (ls res)
	      (deff)
	      (if (or (= 1 (length ls)) (null ls))
		  res
		  (looper (rest (rest ls))
		     (cons (first ls) res)))))
    (looper (reverse xs) nil)))

(defmacro cloop (lbinding ldef lbody)
  `(labels ((recur ,(take-odd lbinding) ,ldef
		   ,lbody))
     ,(cons 'recur (take-even lbinding))))

(defmacro clet (lbinding &body lbody)
  `(let* ,(cmap (fn2 (list %1 %2))
		(take-odd lbinding)
		(take-even lbinding))
     ,@lbody))

(defun inc (x) (1+ x))

(defun dec (x) (1- x))

(defun div (a m)
  (floor (/ a m)))

(defclass lazy-seq ()
  ((val :initarg :val)))

(defmacro lseq (thead &rest ttail)
  `(make-instance 'lazy-seq
		  :val (cons ,thead (lambda () (list ,@ttail)))))

(defmacro lcons (thead ttail)
  `(make-instance 'lazy-seq
		  :val (cons ,thead (lambda () ,ttail))))

(defgeneric empty? (lseq))
(defgeneric head (lseq))
(defgeneric tail (lseq))
(defgeneric rev (lseq))
(defgeneric clast (lseq))

(defgeneric force (lseq))

(defmethod empty? ((lst lazy-seq))
  nil)

(defmethod empty? ((lst list))
  (null lst))

(defmethod head ((lst lazy-seq))
  (deff)
  (first (slot-value lst 'val)))

(defmethod head ((lst list))
  (deff)
  (first lst))

(defmethod tail ((lst lazy-seq))
  (deff)
  (funcall (rest (slot-value lst 'val))))

(defmethod tail ((lst list))
  (deff)
  (rest lst))

(defmethod force ((lst lazy-seq))
  (deff)
  (cons (head lst) (tail lst)))

(defmethod rev ((lst lazy-seq))
  (deff)
  (reverse (cons (head lst) (tail lst))))

(defmethod rev ((lst list))
  (deff)
  (reverse lst))

(defmethod clast ((lst lazy-seq))
  (deff)
  (first (last (tail lst))))

(defmethod clast ((lst list))
  (deff)
  (first (last lst)))

(defun iterate (f i)
  (deff)
  (lcons i (iterate f (funcall f i))))

(defun lrange (&optional start step)
  (deff start step)
  (if step
      (lcons start (lrange (+ start step) step))
      (if start
	  (lcons start (lrange (inc start) 1))
	  (lcons 0 (lrange 1 1)))))

(defmacro range (a &optional b step)
  (if b
      (if step
	  (if (< a b)
	      `(loop for i from ,a to ,b by ,step collect i)
	      `(loop for i from ,a downto ,b by ,step collect i))
	  `(range ,a ,b 1))
      `(range 0 ,a 1)))

(defun take (n lst)
  (deff n)
  (if (or (empty? lst) (= n 0))
      '()
      (cons (head lst) (take (dec n) (tail lst)))))

(defun drop (n lst)
  (deff n)
  (if (or (empty? lst) (= 0 n))
      lst
      (drop (dec n) (tail lst))))

(defmacro if-not (conds ergo alter)
  `(if ,conds ,alter ,ergo))

(defun lmap (f lst)
  (deff)
  (if-not (empty? lst)
	  (lcons (funcall f (head lst))
		 (lmap f (tail lst)))
	  nil))

(defgeneric filter (f lseq))

(defmethod filter (f (lst list))
  (deff)
  (remove-if-not f lst))

(defmethod filter (f (lst lazy-seq))
  (deff)
  (if (funcall f (head lst))
      (lcons (head lst) (filter f (tail lst)))
      (filter f (tail lst))))

(defun take-while (f lst)
  (deff)
  (if (empty? lst)
      nil
      (if (funcall f (head lst))
	  (cons (head lst) (take-while f (tail lst)))
	  (take-while f (tail lst)))))

(defun drop-while (f lst)
  (deff)
  (if (empty? lst)
      nil
      (if (funcall f (head lst))
	  (take-while f (tail lst))
	  (tail lst))))

(defgeneric keep (f lseq))

(defmethod keep (f (lst list))
  (deff)
  (if (empty? lst)
      nil
      (if (funcall f (head lst))
	  (cons (funcall f (head lst)) (keep f (tail lst)))
	  (keep f (tail lst)))))

(defmethod keep (f (lst lazy-seq))
  (deff)
  (if (funcall f (head lst))
      (lcons (funcall f (head lst)) (keep f (tail lst)))
      (keep f (tail lst))))

(defun sort-by (f lst)
  (deff)
  (sort lst '< :key f))

(defun max-by (f lst)
  (if (null lst)
      -99999999999999999999999999999999999999999999999
      (clet (num (funcall f (first lst))
		 nres (max-by f (rest lst)))
	(if (> num nres)
	    num
	    nres))))

(defun min-by (f lst)
  (if (null lst)
      99999999999999999999999999999999999999999999999
      (clet (num (funcall f (first lst))
		 nres (min-by f (rest lst)))
	(if (< num nres)
	    num
	    nres))))

(defun div? (a b)
  (deff a b)
  (zerop (rem a b)))

(defun prime? (p)
  (deff p)
  (cond ((< p 2) false)
	((= 2 p) true)
	((evenp p) false)
	(:else (clet (lim (sqrt p))
		 (cloop (i 3) (deff i)
			(if (> i lim)
			    true
			    (if (div? p i)
				false
				(recur (+ 2 i)))))))))

(defun fold (f g lst)
  (deff)
  (cloop (i (head lst) xs (tail lst)) (deff)
	 (if (funcall g i)
	     i
	     (recur (funcall f i (head xs))
		    (tail xs)))))

(defun flatten (lst)
  (deff)
  (if (empty? lst)
      nil
      (clet (flst (first lst))
	(if (listp flst)
	    (append (flatten (tail lst))
		    (flatten flst))
	    (cons flst (flatten (tail lst)))))))

(defun distinct (lst)
  (deff)
  (remove-duplicates lst))

(defmacro sum (lst)
  `(loop for i in ,lst
      summing i into sumi
      finally (return sumi)))








