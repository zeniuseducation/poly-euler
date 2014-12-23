(defun fibo (a b i lim)
  (if (> a lim) i (fibo (+ a b) a (1+ i) lim)))

(defun sol48 (i res lim buk)
  (if (> i lim)
      res
      (sol48 (1+ i)
	     (rem (+ res (rem (expt i i) buk)) buk)
	     lim buk)))
