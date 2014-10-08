# Polyglottic Euler in 5 Lambdas

Polyglottic attempts to euler in Clojure, Common Lisp (SBCL), Racket, SML and
Haskell. The codes will be refactored from time to time without 
deleting old codes. 

<img src="https://sites.google.com/site/adidozsa2/KeepLisping.jpg">

Note: The logo was taken from <a href="https://sites.google.com/site/adidozsa2/lisp">This blog</a>. Unfortunately nobody noticed that the Johnnie posture is lambda-shaped :)

## About the Lisps

"*Lisp is worth learning for the profound enlightenment experience you
will have when you finally get it; that experience will make you a
better programmer for the rest of your days, even if you never
actually use Lisp itself a lot*." - Eric Raymond, "How to Become a
Hacker"

## Rationales

1. The need for benchmark for a particular euler problem in terms of speed
2. Encouragement for others to come up with faster/better solutions
3. PE problems are good practice for optimising codes, which is critical skill in the era of big-data

In every source code, time-elapsed for each solution will be listed.

## Performance Comparison on MBA i5 1.6GHz

Implementations:  
1. Clojure using Clojure on JVM  
2. Clojurescript on NodeJS  
3. Common Lisp using SBCL  
4. Haskell using GHC (currently still in interpreted mode, will be moved to compiled one)  
5. SML using SMLNJ  
6. Racket using Racket  

##### Problem no 1

Prob: Find the sum of all multiples of 3 or 5 that less than 1000

Clojure averaging 3.2 msecs  
SBCL averaging 0.3 msecs  
Haskell averaging 10 msecs  
Racket 1 msec

##### Problem no 2

Prob: Find the sum of even-valued fibo numbers less than 4,000,000

Clojure 0.08msecs  
SBCL less than 0.01msecs   
Haskell ~10msecs  
Racket less than 1 msec  

##### Problem no 3

Prob: Find the largest prime factor of 600851475143

Clojure 32 msecs  
SBCL 37-42 msecs  
Haskell 1,420 msecs  
Haskell compiled optimised less than 10 msecs  
SML 2 msecs!!! FASTEST ONE!
Racket 50-60 msecs

##### Problem no 4

Prob: Find the largest palindrom that is a product of two 3 digits
number (can be different numbers).

Clojure 16-18 msecs  
SBCL 11-20 msecs  
Haskell 30 msecs  
SML 3 msec -> FASTEST!!!  

##### Problem no 5

Prob: Find the smallest number that can be evenly divided by all
integers from 1 to 20.

Clojure 0.3 msecs  
SBCL 0.04 msecs  
Haskell ~10msecs  

##### Problem no 6

Prob: Calculate the difference between the sum of squares of the first 100 natural numbers and the square of sum those numbers.

Clojure 0.3-0.4 msecs  
SBCL 0.01 msecs  
Haskell (it seems that GHCi cannot time under 10msecs)  

##### Problem no 7

Prob: Find the 10,001st prime  

Clojure 40-50 msecs  
SBCL 90-110 msecs  
Haskell 30 msecs  
SML 17 msecs!!!  Again the FASTEST one!!!  

##### Problem no 8

Prob: Find the largest product of 13 digits in a 1000 digits series

Clojure 45-65 msecs  
SBCL 23 msecs  
Haskell 100 msecs  

##### Problem no 9

Clojure 0.08 msecs  
SBCL 21 msecs  
Haskell 240 msecs  
Haskell compiled 10 ms  
SML 3 ms !! Remarkable considering the bad construct  

##### Problem no 10

Prob : The sum of all positive primes that less than 2,000,000  

Clojure 2,500 msecs  (2.5 secs)  
SBCL 5,800 msecs  (5.8 secs)  edit 3,269ms in SBCL 1.2.2  
Haskell 1,320 msecs (1.3 secs) -> 80 ms with in-memory lazy sieve  
SML 736 msecs!! (0.736 secs) FASTEST!  
Racket 4-6K msecs (5-6 secs)  

##### Problem no 12

Prob: Triangle number that has more than 500 factors

Clojure 1.7 seconds   
SBCL 2.4 seconds  
Haskell 85 seconds  
Haskell compiled 0.66 seconds FASTEST SO FAR!!  
SML 0.73 seconds  


##### Problem no 14

Clojure 5.3 seconds  
SBCL 29-32 seconds  
Haskell 271 seconds  
Haskell compiled 4.36 seconds FASTEST !!  
SML 36 seconds  

##### Problem no 15

Clojure 0.53 msec  
SBCL 0.018 msec  

##### Problem no 16

Too easy to measure, all in one lines and took less than 1 ms

##### Problem no 20

Too easy to measure, all in one lines and took less than 1 ms

##### Problem no 21

Clojure 0.07 sec  
SBCL 0.05 sec  
Haskell 2.3 sec  
Haskell compiled 0.03 sec  

##### Problem 24

Problem: Very nice problem, highly recommended!!

Clojure 0.23 msec  
SBCL 0.02 msec  
Haskell 1 msec  

##### Problem no 25

Prob: Find the first fibo element that reach 1000 digits.

Clojure averaging 2.9 msecs  
SBCL averaging 1.3 msecs  
Haskell averaging 20 msecs  

##### Problem 29

Clojure 0.065 msec  
SBCL 0.019 msec  
Haskell (one liner) 1.2 sec

##### Problem no 30

Clojure 0.73 sec  
SBCL 1.3 sec  
Haskell 6.9 sec

##### Problem 33 

SBCL 0.8 msec  

##### Problem no 85

Clojure 1.5 sec  
SBCL 0.53 sec  

##### Problem no 100

Clojure 0.039 msec  
SBCL 1.2 msec  

##### Problem no 125

Clojure 0.5 sec  
SBCL 0.6-0.8 sec  

##### Problem no 173 

Clojure 0.163 sec  
Haskell 8.3 sec  

##### Problem no 174

Clojure 1.2 sec  

##### Problem no 234

Clojure 2.3 sec  
SBCL 1.4 sec  

#####

Note: Haskell on emacs is an interpreted version, when compiled using advance optimisation, the speed could be 10-30times faster.

## License

Copyright © 2014 PT Zenius Education

Distributed under the Eclipse Public License either version 1.0.




