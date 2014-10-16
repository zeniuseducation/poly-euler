
(load "clojure.lisp")

;; PROBLEM 85

(defun csrec (i j m n)
  "Returns the number of possible i x j rectangles plotted into an m x n plane"
  (* (1+ (- m i))
     (1+ (- n j))))

(defun crec (m n)
  "Returns the number of arbitrary-sized rectangles that can be
plotted into m by n plane"
  (sum (loop for i from 1 to m
	  append (loop for j from 1 to n
		    collect (csrec i j m n)))))

(defun sol85 (size lim)
  "Returns the rectangle 'size' which rectangles closest to 2 millions"
  (first (sort (loop for i from 1 to size
		  append (loop for j from 1 to size
			    collect (list (* i j)
					  (abs (- lim (crec i j))))))
	       '< :key 'second)))

(defun euler85 (size lim)
  (time (sol85 size lim)))

"Elapsed time for 90x90 0.57 seconds"

;; PROBLEM 66

(defun step66a (limx)
  (loop for x from 1 to limx
     append (loop for y in (range 1 x)
	       when (integerp (/ (dec (sqr x)) (sqr y)))
	       collect (list x y (/ (dec (sqr x)) (sqr y))))
     do (print x)))



(defun step66b (limx)
  (take 50 (sort (filter #'(lambda (x) (<= (third x) 1000))
			 (remove-duplicates (step66a limx)
					    :from-end t :key 'third))
		 '> :key 'first)))

(defun psqr? (x)
  (let ((px (sqrt x)))
    (= (ceiling px) (floor px))))

(defun finder66 (d x)
  (let ((ysqr (/ (dec (sqr x)) d)))
    (if (psqr? ysqr)
	(list d x (round (sqrt ysqr)))
	(finder66 d (inc x)))))

(defun materials (limx)
  (set-difference (range 1 (inc limx))
		  (mapcar 'sqr (range 1 32))))

(defun step66c (limx)
  (loop for d in (materials limx)
     collect (finder66 d 2)
     do (print d)))

(defun sol66 (limx)
  (time (take 30 (sort (step66c limx) '> :key 'second))))

(defun find-y (d x)
  (sqrt (/ (dec (sqr x)) d)))

;; PROBLEM 76 SUMMATIONS

(defun partition76 (n)
  (div n 2))

(defun sol76 (n)
  (if (= n 2)
      2
      (+ (partition76 n)
	 (div (sum (cmap 'sol76 (range (dec n) (dec (div n 2)) -1))) 2))))

;; PROBLEM 24

(defun lpfactors (n)
  (remove-duplicates
   (let ((pfacts (pfactors n)))
     (mapcar 'product
	     (mapcan #'(lambda (x) (combine pfacts x))
		     (range 1 (inc (length pfacts))))))))






