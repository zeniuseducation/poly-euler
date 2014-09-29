(load "lunity/clojure.lisp")

(defun findbc (a b lim res)
  (let* ((asqr (sqr a))
	 (bsqr (sqr b))
	 (csqr (+ asqr bsqr))
	 (c (round (sqrt csqr))))
    (if (or (> b lim) (> (- (sqr (inc b)) bsqr) asqr))
	res
	(if (and (<= (+ a b c) lim) (psqr? csqr))
	    (findbc a
		    (inc b)
		    lim
		    (cons (list a b (round (sqrt csqr))) res))
	    (findbc a (inc b) lim res))))) 

(defun pita1 (lim)
  (remove nil
	  (apply 'append
		 (loop for a in
		      (primes-under (inc (quot lim 3)))
		    collect (findbc a
				    (inc a)
				    (inc (quot lim 2))
				    nil)))))

(defun findac (b a res)
  (let* ((asqr (sqr a))
	 (bsqr (sqr b))
	 (csqr (+ asqr bsqr))
	 )
    (if (or (<= a 1) (> (- (sqr (inc b)) bsqr) asqr))
	res
	(if (psqr? csqr)
	    (findac b
		    (dec a)
		    (cons (list a b (round (sqrt csqr))) res))
	    (findac b (dec a) res)))))

(defun pita2 (lim)
  (remove nil
	  (apply 'append
		 (loop for b in (primes-under (inc (quot lim 2)))
		    collect (findac b (dec b) nil)))))


(defun pita (lim)
  (loop for a from 1 to (quot lim 3)
     for asqr = (sqr a)
     append (loop for b from (inc a) to (inc (quot lim 2))
	       for bsqr = (sqr b)
	       for csqr = (+ (sqr a) (sqr b))
	       for c = (round (sqrt csqr))
	       while (<= (- (sqr (inc b)) bsqr) asqr)
	       when (and (<= (+ a b c) lim) (psqr? csqr))
	       collect (+ a b c))))

(defun pitalist (lim)
  (loop for a from 1 to (quot lim 3)
     for asqr = (sqr a)
     append (loop for b from (inc a) to (inc (quot lim 2))
	       for bsqr = (sqr b)
	       for csqr = (+ (sqr a) (sqr b))
	       for c = (round (sqrt csqr))
	       while (<= (- (sqr (inc b)) bsqr) asqr)
	       when (and (<= (+ a b c) lim) (psqr? csqr))
	       collect (list a b c))))

(defun savefile (fname lim)
  (with-open-file (outfile fname
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    (prin1 (pita lim) outfile)))



(defun triplet (a)
  (let ((b (/ (dec (sqr a)) 2)))
    (list a b (inc b))))

(defun triplets (lim)
  (mapcar 'sum (mapcar 'triplet (range 3 lim 2))))

(defun tripletss (lim)
  (sort (apply 'append
	  (loop for i from 3 to (+ 100 (sqrt lim)) by 2
	     for x = (sum (triplet i))
	when (<= x lim)
      collect (range x lim x))) '<))





