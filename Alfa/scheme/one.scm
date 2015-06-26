(define lim (expt 10 999))

(define (fibo a b i lim) (if (> a lim) i (fibo (+ a b) a (+ i 1) lim)))
