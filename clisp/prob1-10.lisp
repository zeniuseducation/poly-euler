
;; Load the basic math functionalities

(load "math.lisp")


;; PROBLEM NO 1

(defun euler1 (a b lim)
  (reduce '+ (remove-if-not #'(lambda (x) (or (zerop (rem x a))
					 (zerop (rem x b))))
			    (range 1 lim))))

"Elapsed time almost zero"

;; CL-USER> (time (euler1 3 5 1000))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000281 seconds of total run time
;;   100.00% CPU
;;   443,738 processor cycles
;;   32,768 bytes consed

(defun euler2 (ls res lim)
  "Returns the sum of all even valued elements of fibo numbers less
  than lim"
  (if (>= (first ls) lim)
      res
      (euler2 (list (+ (first ls)
		       (second ls))
		    (first ls))
	      (if (evenp (first ls))
		  (+ (first ls) res)
		  res)
	      lim)))

"Elapsed time : in a snap!!"

;; PROBLEM 3

(defun euler3 (n)
  (apply 'max
	 (remove-if-not 'prime?
			(factors n))))

"the number 600851475143"

"Elapsed time 37-42 msecs"

(defun palin? (n)
  (let ((st (write-to-string n)))
    (equal st (reverse st))))

(defun euler4 (start end)
  (apply 'max
	 (loop for x from start to end
	    append (loop for y from x to end
		      when (palin? (* x y))
		      collect (* x y)))))

"Elapsed time 150msecs"

;; CL-USER> (time (euler4 900 1000))
;; Evaluation took:
;;   0.015 seconds of real time

;; PROBLEM NO 5

(defun rude-lcm (ls res) 
  (let ((a (first ls))
	(xs (rest ls)))
    (if (null xs)
	(cons a res)
	(if (some #'(lambda (x) (zerop (rem x a))) xs)
	    (rude-lcm (mapcar #'(lambda (x) (if (zerop (rem x a))
					   (/ x a)
					   x))
			      xs)
		      (if (prime? a) (cons a res) res))
	    (rude-lcm xs (cons a res))))))

(defun euler5a (n)
  (apply '* (rude-lcm (range 1 (1+ n)) nil)))

"Elapsed time : ultra fast!!"


;; CL-USER> (time (euler5a 20))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000035 seconds of total run time
;; NOTES: SBCL is REALLY FAST for this kind of thing

(defun euler6 (n)
  (- (sum (mapcar 'sqr (range 1 (1+ n)))) (sqr (sum (range 1 (1+ n))))))

"Elapsed time : 0.031 msecs"

(defun euler7-helper (n i p)
  (if (= n i) p (euler7-helper n (1+ i) (next-prime p))))

(defun euler7 (n)
  "Returns the nth positive prime number"
  (euler7-helper n 1 2))

"Elapsed time 109 msecs"

;; PROBLEM no 8

(defconstant numbers 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)

(defun numcol (n)
  (if (< n 10)
      (list n)
      (cons (rem n 10) (numcol (quot n 10)))))

(defun euler8-helper (ls n mx)
  (if (= n (length ls))
      (max mx (product ls))
      (euler8-helper (rest ls) n (max mx (product (take n ls))))))

(defun euler8 (n)
  (euler8-helper (numcol n) 13 0))

"Elapsed time : 23 msecs"

;; PROBLEM NO 9

(defun euler9 (n)
  (loop for a from 1 to n
     append (loop for b from a to n
	       when (= (* (- n a b) (- n a b))
		       (+ (* a a) (* b b)))
	       collect (* a b (- n a b)))))

"Elapsed time 21 msecs"

;; CL-USER> (time (euler9 1000))
;; Evaluation took:
;;   0.021 seconds of real time
;;   0.021759 seconds of total run time (0.021686 user, 0.000073 system)
;;   104.76% CPU
;;   34,766,596 processor cycles
;;   0 bytes consed


(defun sum-primes (n p res)
  (if (>= p n)
      res
      (sum-primes n (next-prime p) (+ res p))))

(defun euler10 (n)
  (sum-primes n 2 0))

"Elapsed time 5,750 msecs or 5.7 secs"





