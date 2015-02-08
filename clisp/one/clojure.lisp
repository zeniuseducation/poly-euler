(defun clojure-vector-char (stream char)
  `',(read-delimited-list #\] stream t))

(set-macro-character #\[ #'clojure-vector-char)
(set-macro-character #\] (get-macro-character #\)))

(defmacro defn (fname fbind fbody)
  `(defun ,fname ,(list @fbind)
     (declare (optimize (speed 3)))
     ,fbody))

(defmacro deff (&rest ftypes)
  (declare (optimize (speed 3))
	   `(fixnum @ftypes)))

(defmacro fn (body)
  `(lambda (%) ,body))

(defmacro fn2 (body)
  `(lambda (%1 %2) ,body))

(defun cmap (f lst)
  (mapcar f lst))

(defun inc (x) (1+ x))
(defun dec (x) (1- x))

(defun div (a m)
  (truncate (/ a m)))

(defun range (&rest args)
  (deff)
  (cond ((= 1 (length args))
	 (loop for i from 0 to (first args) collect i))
	((= 2 (length args))
	 (loop for i from (first args) to (second args) collect i))
	((= 3 (length args))
	 (let ((a (first args))
	       (b (second args)))
	   (if (<= a b)
	       (loop for i from a to b by (third args) collect i)
	       (loop for i from a downto b by (third args) collect i))))))

(defmacro ->> (&body body)
  (labels ((looper (ls res)
	      (if (null ls)
		  res
		  (looper (rest ls)
		     (append (first ls) (list res))))))
    (looper (rest body) (first body))))

(defmacro -> (&body body)
  (labels ((looper (ls res)
	      (if (null ls)
		  res
		  (looper (rest ls)
		     (append (list (first (first ls)))
			     (list res)
			     (rest (first ls)))))))
    (looper (rest body) (first body))))

(defun hd (lst)
  (first lst))

(defun clast (lst)
  (car (last lst)))

(defun take (n xs)
  (deff n)
  (labels ((looper (i lxs res)
	      (deff i)
	      (if (or (null lxs) (> i n))
		  res
		  (looper (inc i)
		     (rest lxs)
		     (append res (list (first lxs)))))))
    (looper 1 xs nil)))

(defun drop (n xs)
  (deff n)
  (if (= 0 n)
      xs
      (drop (dec n) (rest xs))))

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

(defun take-while (f lst)
  (cloop (lxs lst res nil) (deff)
    (cond ((null lxs) res)
	  ((not (funcall f (first lxs))) res)
	  (:else (recur (rest lxs)
			(append res (list (first lxs))))))))

(defmacro clet (lbinding &body lbody)
  `(let* ,(mapcar #'(lambda (a b) (list a b))
		  (take-odd (second lbinding))
		  (take-even (second lbinding)))
     ,@lbody))

(defun drop-while (f lst)
  (if (null lst)
      '()
      (if (funcall f (first lst))
	  (drop-while f (rest lst))
	  lst)))

(defun repeat (n m)
  (deff)
  (if (zerop n)
      nil
      (cons m (repeat (1- n) m))))

(defun insert (elm n xs)
  (->> (drop (1- n) xs)
       (cons elm)
       (append (take (1- n) xs))))










