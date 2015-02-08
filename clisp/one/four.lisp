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

(defun sol94 (lim)
  (deff lim)
  (cloop (i 2 res 0) (deff i res)
	 (if (> (inc (* 3 i)) lim)
	     res
	     (let* ((a (inc i))
		    (b (dec i))
		    (p1 (/ (+ i i a) 2))
		    (p2 (/ (+ i i b) 2))
		    (asq (* p1 (- p1 i) (- p1 i) (- p1 a)))
		    (bsq (* p2 (- p2 i) (- p2 i) (- p2 b))))
	       (if (psqr? asq)
		   (if (psqr? bsq)
		       (recur (inc i)
			      (+ res
				 (+ (* 2 i) a)
				 (+ (* 2 i) b)))
		       (recur (inc i) (+ res (+ (* 2 i) a))))
		   (if (psqr? bsq)
		       (recur (inc i) (+ res (+ (* 2 i) b)))
		       (recur (inc i) res)))))))

(defun prime? (p)
  (deff p)
  (if (evenp p)
      nil
      (cloop (i 3) (deff i)
	     (cond ((> (* i i) p) t)
		   ((= 0 (rem p i)) nil)
		   (t (recur (+ i 2)))))))

(defun permute (ls)
  "Returns all possible permutations of ls"
  (if (= 1 (length ls))
      (mapcar 'list ls)
      (loop for i in ls
	 append (loop for rs in (permute (remove i ls))
		   collect (cons i rs)))))

(defun combine (ls n)
  "Takes n combinations of ls"
  (if (= 0 n)
      '(())
      (loop for i in ls
	 for j from 1 to (length ls)
	 append (loop for rs in (combine (drop j ls)
					 (dec n))
		   collect (cons i rs)))))

(defun k-permute (xs n)
  (deff n)
  (mapcan (fn (permute %)) (combine xs n)))

(defun selipin (xs rl pl)
  (let* ((bahan (mapcar 'list rl pl)))
    (-> (fn2 (insert (first %2)
		     (second %2)
		     %1))
	(reduce bahan :initial-value xs))))

(defun primes-d (dig n t)
  (let* ((bahan (range 10))
	 (sempalan (repeat n dig))
	 (digits (range 1 t)))
    (cloop (korban sempalan res nil)
	   )))







