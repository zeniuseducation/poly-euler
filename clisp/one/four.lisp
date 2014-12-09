(load "clojure.lisp")

(defun sqrs (lim)
  (deff lim)
  (->> (range lim)
       (cmap (fn (* % %)))
       (reduce '+)))

(defun cubes (lim)
  (deff lim)
  (-> (fn (* % % %))
      (cmap (range lim))))

(defun elon (lim)
  "Generates the list for e"
  (deff lim)
  (->> (range 2 lim 2)
       (mapcan (fn (list 1 % 1)))
       (cons 2)
       (take lim)))

(defun expand (xs)
  "Create an expansion based on xs which is the list"
  (deff)
  (if (null (rest xs))
      (first xs)
      (+ (first xs) (/ 1 (expand (rest xs))))))

(defun sumdigs (i)
  "Sum of digits"
  (deff)
  (if (< i 10)
      i
      (+ (rem i 10)
	 (sumdigs (div i 10)))))

(defun euler65 (lim)
  "First, generates the list, creates fraction based on the list, take
numerator of the fraction and sum the digits"
  (deff lim)
  (->> (elon lim)
       (expand)
       (numerator)
       (sumdigs)))

(defun numcol- (n)
  (deff)
  (labels ((looper (i res)
	      (if (< i 10)
		  (cons i res)
		  (looper (div i 10)
		     (cons (rem i 10) res)))))
    (looper n nil)))

(defun numcol (n)
  (deff)
  (cloop (i n res nil) (deff)
    (if (< i 10)
	(cons i res)
	(recur (div i 10)
	       (cons (rem i 10) res)))))

(defun psqr? (n)
  (let ((num (* 1.0l0 (sqrt n))))
    (= (ceiling num) (floor num))))

(defun colnum- (ls)
  (deff)
  (labels ((looper (ls res)
	      (if (null ls)
		  res
		  (looper (rest ls) (+ (* res 10) (first ls))))))
    (looper ls 0)))

(defun colnum (ls)
  (deff)
  (cloop (xs ls res 0) (deff)
    (if (null xs)
	res
	(recur (rest xs) (+ (* res 10) (first xs))))))

(defun sdigits- (n lim)
  (deff n lim)
  (labels ((looper (res digs)
	      (deff digs)
	      (labels ((inner (tr i curs)
			 (deff i curs)
			 (let* ((num (colnum (append res (list i))))
				(nums (* num num))
				(diff (- tr nums)))
			   (if (> diff 0)
			       (inner tr (inc i) i)
			       (append res (list curs))))))
		(if (= digs lim)
		    res
		    (let ((tar (* n (expt 10 (* 2 digs)))))
		      (looper (inner tar 0 0)
			 (inc digs)))))))
    (looper nil 0)))

(defun sdigits (n lim)
  (deff n lim)
  (cloop (res nil digs 0) (deff)
    (labels ((inner (tr i curs)
	       (deff i curs)
	       (clet [num (colnum (append res (list i)))
		     nums (* num num)
		     diff (- tr nums)]
		     (if (> diff 0)
			 (inner tr (inc i) i)
			 (append res (list curs))))))
      (if (= digs lim)
	  res
	  (clet [tar (* n (expt 10 (* 2 digs))) ]
		(recur (inner tar 0 0)
		       (inc digs)))))))

(defun euler80 (lim)
  (deff lim)
  (->> (range 2 lim)
       (remove-if (fn (psqr? %)))
       (cmap (fn (reduce '+ (sdigits % 100))))
       (reduce '+)))


