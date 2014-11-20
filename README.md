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

Clojure 0.08 msecs  
SBCL less than 0.01 msecs   
Haskell ~10 msecs  
Racket less than 1 msec  

##### Problem no 3

Prob: Find the largest prime factor of 600851475143

Clojure 32 msecs  
Type hinted Clojure using transient and unchecked-math 5-6msecs  
SBCL 37-42 msecs  
Type hinted SBCL 4-6ms  
Haskell 1,420 msecs   
Haskell compiled optimised less than 10 msecs  
SML 2 msecs!!! FASTEST ONE!  
Racket 50-60 msecs  
Typed Racket 6ms  

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
Type-hinted Clojure 30-50 ms  
Type-hinted Clojure using sieve 5-8ms  
SBCL 90-110 msecs  
Type hinted SBCL 24ms  
Type-hinted SBCL using sieves 2ms  
Typed Racket 35-38ms  
Typed Racket using sieve  2-4ms  
Haskell 30 msecs   
SML 17 msecs

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
Type Hinted Clojure using *unchecked-math* 1,165 msecs  
Type-hinted Clojure using sieves 120-140ms  
SBCL 3,269ms in SBCL 1.2.2  
Type hinted SBCL 1,236ms  
Type-hinted SBCL using sieves 30-40ms  
Haskell 1,320 msecs (1.3 secs)  
Haskell on Win32 470 msecs
SML 736 msecs!! (0.736 secs)   
Racket 4-6K msecs  
Typed Racket 2,200 msecs (Faster than Clojure)  
Typed Racket in Win32 987 msecs (FASTEST LISP)  
Typed Racket using sieves 71-76ms  
C Benchmark 38 ms  

##### Problem no 12

Prob: Triangle number that has more than 500 factors

Clojure 1.7 seconds   
Type hinted Clojure 0.56 seconds  
SBCL 2.4 seconds  
Type hinted SBCL 0.59 seconds  
Typed Racket 1 sec  
Haskell 85 seconds  
Haskell compiled 0.66 seconds FASTEST SO FAR!!  
SML 0.73 seconds  


##### Problem no 14

Clojure 5.3 seconds  
Type hinted Clojure 1.2-1.3 seconds  
Type hinted Clojure using memoize 0.8-0.9 second  
Type hinted Clojure starting 500,000 and odd-numbers only using memoize 168ms  
SBCL 29-32 seconds  
Type hinted SBCL 5 seconds  
Type hinted SBCL 1.3 sec  
Haskell compiled 4.36 seconds  
Typed Racket 3.3 seconds (even better than Haskell)  
SML 36 seconds  

##### Problem no 15

Clojure 0.53 msec  
SBCL 0.018 msec  

##### Problem no 16

Too easy to measure, all in one lines and took less than 1 ms

##### Problem no 20

Too easy to measure, all in one lines and took less than 1 ms

##### Problem no 21

Problem : The sum of all amicable numbers less than 10,000  

Clojure 72 ms    
Type hinted Clojure 17-22 ms  
SBCL 51 ms    
Type hinted SBCL 17-20 ms  
Haskell compiled 30 ms  
Typed Racket 24ms  
SML 23 ms  

##### Problem 24

Problem: Very nice problem, highly recommended!!

Clojure 0.23 msec  
SBCL 0.02 msec  
Haskell 1 msec  

##### Problem no 25

Prob: Find the first fibo element that reach 1000 digits.

Clojure averaging 2.9 msecs  
Type hinted Clojure using *unchecked-math* < 1 ms  
SBCL averaging 1.3 msecs   
Racket 3-4 msecs  
Typed Racket 1 ms  
Haskell averaging 10 msecs  
SML 36 msecs  (SML LargeInt is not efficient)

##### Problem 29

Clojure 0.065 msec  
SBCL 0.019 msec  
Haskell (one liner) 1.2 sec  
SML 2.4 sec (Very inefficient sml)  

##### Problem no 30

Clojure 0.73 sec  
SBCL 1.32 sec  
Haskell 6.91 sec  
Haskell compiled 0.32 sec  
SML 1.62 sec  

##### Problem 32

Prob: axb = c where a++b++c pandigital 1-9  

Haskell compiled 19 sec  

##### Problem 33 

SBCL 0.8 msec  

##### Problem 34

Prob: find the sum of all numbers that are equal to the sum of factorials of their digits  

SBCL 0.76 sec  
Haskell compiled 0.46sec & 5.3 sec depends on limit  
SML 0.349 & 1.349 depends on limit  

##### Problem no 35

Prob: how many circular primes exist under 1000000  

SBCL 1.46 sec  
Haskell compiled 0.55 sec FAST!!  
SML 0.36 sec FASTEST!!  

##### Problem no 36 

Prob: find the sum of all numbers that are palindrome in both decimal and binary bases  

SBCL 4.7 sec  
Haskell 0.77 sec  
SML 0.86 sec  

##### Problem no 37

SBCL 1.58sec  
Clojure 1.5sec  
Haskell 0.72sec  

##### Problem no 39

SBCL 8ms  
Clojure 12ms  
Haskell 10ms  

##### Problem no 40

Haskell 20ms  
Clojure 31ms  

##### Problem no 41

Haskell (one liner) 10ms  

##### Problen no 51

Haskell 320ms

##### Problem no 85

Clojure 1.5 sec  
SBCL 0.53 sec  

##### Problem no 100

Clojure 0.039 msec  
SBCL 1.2 msec  

##### Problem no 125

Clojure 0.5 sec  
SBCL 0.6-0.8 sec  

##### Problem no 131

SBCL 3ms  
Haskell interpreted 230ms  

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






