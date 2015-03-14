# Polyglottic Euler in 5 Lambdas

Polyglottic attempts to euler in Clojure, Common Lisp (SBCL), Racket, SML and
Haskell. The codes will be refactored from time to time without 
deleting old codes. 

<img src="https://sites.google.com/site/adidozsa2/KeepLisping.jpg">

Note: The logo was taken from <a href="https://sites.google.com/site/adidozsa2/lisp">This blog</a>. 
Unfortunately nobody noticed that the Johnnie posture is lambda-shaped :)

### The Eulerians:  
<img src="https://projecteuler.net/profile/squest.png">
<img src="https://projecteuler.net/profile/skadinyo.png">
<img src="https://projecteuler.net/profile/memeri.png">

## About the Lisps

"*Lisp is worth learning for the profound enlightenment experience you
will have when you finally get it; that experience will make you a
better programmer for the rest of your days, even if you never
actually use Lisp itself a lot*."   
- Eric Raymond, "How to Become a Hacker"

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
4. Haskell using GHC compiled using -O2 optimisation   
5. SML using SMLNJ  
6. Racket mostly using Typed Racket  
7. C using whatever there is on Macbook, optimized to the max using -Ofast  

##### Problem no 1

Prob: Find the sum of all multiples of 3 or 5 that less than 1000

Clojure averaging 3.2 msecs  
Clojure 1.7 0.08ms  
SBCL averaging 0.3 msecs  
Haskell averaging 10 msecs  
Racket 1 msec  
Java benchmark 0.05ms  
C benchmark 0.05ms  
Swift 0.018ms  
Julia 0.008ms NEW RECORD!  

##### Problem no 2

Prob: Find the sum of even-valued fibo numbers less than 4,000,000

Clojure 0.08 msecs  
Clojure 1.7 0.04ms  
SBCL less than 0.01 msecs   
Haskell ~10 msecs  
Racket less than 1 msec  
C Benchmark 0.03ms  
Java benchmark 0.005ms  
Swift 0.004ms  
Julia 0.004ms TIGHT with SWIFT!  

##### Problem no 3

Prob: Find the largest prime factor of 600851475143

Clojure 32 msecs  
Type hinted Clojure using transient and unchecked-math 5-6msecs  
Further optimised type-hinted Clojure 0.3-0.4ms  
SBCL 37-42 msecs  
Type hinted SBCL 4-6ms  
Further optimised SBCL 0.45ms  
Haskell compiled optimised less than 10 ms    
SML 2 ms  
Racket 50-60 ms  
Typed Racket 6ms   
Further optimised Typed Racket 0.4-0.5ms  
C as benchmark 0.2ms  
Java as benchmark 5ms  
Swift 0.11ms NEW RECORD!!  
Julia 0.16ms  

##### Problem no 4

Prob: Find the largest palindrom that is a product of two 3 digits
number (can be different numbers).

Clojure 16-18 msecs  
Clojure with memoization 0.01ms  
SBCL 11-20 msecs  
Haskell 30 msecs  
SML 3 msec  
Julia 0.006ms  FAST! (using memoization)  

##### Problem no 5

Prob: Find the smallest number that can be evenly divided by all
integers from 1 to 20.

Clojure 0.3 msecs  
SBCL 0.04 msecs  
Haskell ~10msecs  
Julia 0.01ms  

##### Problem no 6

Prob: Calculate the difference between the sum of squares of the first 100 natural numbers and the square of sum those numbers.

Clojure 0.3-0.4 msecs  
SBCL 0.01 msecs  
Haskell (it seems that GHCi cannot time under 10msecs)  
Julia 0.004ms  

##### Problem no 7

Prob: Find the 10,001st prime  

Clojure 40-50 msecs  
Type-hinted Clojure 30-50 ms  
Type-hinted Clojure using sieve 5-8ms  
Type-hinted Clojure 1.7 using transducers 0.4ms    
SBCL 90-110 msecs  
Type hinted SBCL 24ms  
Type-hinted SBCL using sieves 2ms  
Typed Racket 35-38ms  
Typed Racket using sieve  2-4ms  
Haskell 30 msecs   
SML 17 msecs  
C Benchmark using sieve 0.9ms  
Swift naive using Int32 10ms  
Swift using sieves 0.7ms  
Julia using sieves 0.7ms  

##### Problem no 8

Prob: Find the largest product of 13 digits in a 1000 digits series

Clojure 45-65 msecs    
Clojure 1.7 3ms    
SBCL 23 msecs  
Haskell 100 msecs  
Haskell 3ms  

##### Problem no 9

Type-hinted Clojure 3-4ms  
Further optimised Clojure 0.4ms  
Crazy clojure technique 0.06ms  
Type-hinted SBCL 3ms  
Typed Racket 1ms  
Haskell <10 ms  
SML 1 ms  
C as benchmark 0.25ms  
Java benchmark 2.5ms  
Swift 0.32 ms  

##### Problem no 10

Prob : The sum of all positive primes that less than 2,000,000  

Clojure 2,500 msecs  (2.5 secs)  
Type Hinted Clojure using *unchecked-math* 1,165 msecs  
Typed Hinted Clojure naive using clojure-1.7 pmap-reducers 360ms (FASTEST for non-sieve)  
Type-hinted Clojure using sieves 120-140ms  
Type-hinted Clojure 1.7 sieve using reducers 15ms!!   
Clojure 1.7 sieve 6ms!! FASTEST!!  
SBCL 3,269ms in SBCL 1.2.2  
Type hinted SBCL 1,236ms  
The same type hinted version in 32bit windows runs in 832ms   
Type-hinted SBCL using sieves 30-40ms  
Haskell 1,320 msecs (1.3 secs)  
Haskell on Win32 470 msecs   
SML 736 msecs!! (0.736 secs)    
Racket 4-6K msecs  
Typed Racket 2,200 msecs (Faster than Clojure)  
Typed Racket in Win32 987 msecs  
Typed Racket using sieves 71-76ms  
C Benchmark using sieve 24 ms  
Swift naive 1,421ms  
Swift sieves 16ms  
Julia using sieves 19ms  

##### Problem no 12

Prob: Triangle number that has more than 500 factors

Clojure 1,700 ms    
Type hinted Clojure 560 ms    
Clojure v2 using memoized 13ms    
SBCL 2,400 ms    
Type hinted SBCL 590 ms    
SBCL v2 21 ms    
SBCL using proto-memoize 1ms  
Typed Racket 1000 ms    
Racket v2 42 ms    
Haskell compiled 660 ms      
Haskell v2 30ms  
SML 730ms    
SML v2 13 ms    
C Benchmark 580 ms  
C Benchmark v2 6 ms  
C Benchmark proto-memoize 3 ms  
Java Benchmark 11 ms  
Swift 7ms  
Julia 9ms  


##### Problem no 14

Clojure 5.3 seconds  
Type hinted Clojure 1.2-1.3 seconds  
Type hinted Clojure using memoize 0.8-0.9 second  
Type hinted Clojure starting 500,000 and odd-numbers only using memoize 168ms  
SBCL 29-32 seconds  
Type hinted SBCL 5 seconds  
Type hinted SBCL 1.4 sec  
Haskell compiled 4.36 seconds  
Typed Racket 3.3 seconds (even better than Haskell)  
Typed Racket 0.9 second (check >500k odds only)  
SML 36 seconds  
C as benchmark (starting from half a million and check odd only) 179ms   
Swift 185ms  
Julia 800ms  

##### Problem no 15

Clojure 0.53 msec  
SBCL 0.018 msec  

##### Problem no 16

Too easy to measure, all in one lines and took less than 1 ms  

##### Problem 18

Julia 0.2ms  
Haskell 1ms  

##### Problem no 20

Too easy to measure, all in one lines and took less than 1 ms

##### Problem no 21

Problem : The sum of all amicable numbers less than 10,000  

Clojure 72 ms    
Type hinted Clojure 17-22 ms  
Clojure 1.7 11ms  
SBCL 51 ms    
Type hinted SBCL 17-20 ms  
Haskell compiled 30 ms  
Typed Racket 24ms  
SML 23 ms  
Java benchmark 14ms  
C as benchmark 7ms   
C using dynamic programming 4.5ms   

##### Problem 22 

Haskell 90ms  
Julia 23ms  

##### Problem 23 

Problem : The sum of all integers that cannot be written as the sum of two abundant numbers  

Type-hinted Clojure 240-250ms  
Clojure 1.7 110ms  
Type-hinted SBCL 275ms  
Typed Racket 450-460ms  
C as benchmark 128ms    

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
SBCL One-liner 1ms  
Racket 3-4 msecs  
Typed Racket 1 ms  
Haskell averaging 10 msecs  
SML 36 msecs  (SML LargeInt is not efficient)

##### Problem no 26 

Prob: Very nice problem, find the max recurring digits of recyprocal 1/d where d < 1  

Type-hinted Clojure 0.5-0.6ms  
Type-hinted SBCL 1.2ms  
Typed Racket 0.6-0.8ms  
SML 0.4-0.6ms  
C as benchmark 0.12ms  
Python for benchmark 8.7ms  

##### Problem 27

Prob: Pretty complex to write here  

Clojure 27-28ms  
SBCL 36-38ms  
SBCL using proto-memoize 10ms  
Haskell 80ms  
SML 3,290ms (Very sloow)  
C as benchmark 20ms    

##### Problem 28

Prob: Some fun stuffs  

Clojure 0.082ms  
SBCL 0.023ms  
Haskell < 10ms  
C as benchmark 0.042ms  

##### Problem 29

Clojure 0.065 msec  
SBCL 0.019 msec  
Haskell (one liner) 1.2 sec  
SML 2.4 sec (Very inefficient sml)  

##### Problem no 30

Clojure 730ms    
Parallel mapping Clojure 92ms  
Type-hinted SBCL 820ms  
Haskell compiled 320ms  
New version Haskell 170ms  
SML 1,620ms  
SML new version 315ms  
C Benchmark 287ms     

##### Problem 31

Problem : How many ways we can write 200 as the sum of coins 1,2,5,10,20,50,100,200  

Clojure using memoization 7.2ms & 0.1ms for subsequent calls  
Haskell 20-30ms  
SBCL 4ms  
SML 3ms  
C Benchmark 0.5ms  

##### Problem 32

Prob: axb = c where a++b++c pandigital 1-9  

Clojure 120ms  
Common Lisp 57ms  
Haskell compiled 19 sec  
Haskell v2 ~60ms  
SML 77ms  

##### Problem 33 

SBCL 0.8 msec  

##### Problem 34

Prob: find the sum of all numbers that are equal to the sum of factorials of their digits  

SBCL 0.76 sec  
Haskell compiled 0.46sec & 5.3 sec depends on limit  
SML 0.349 & 1.349 depends on limit  

##### Problem no 35

Prob: how many circular primes exist under 1000000   

Clojure 56ms  
Clojure using parallel map 21ms  
SBCL 1,460ms  
Common Lisp (SBCL) v2 22ms  
SBCL proto-memoize 12ms  
Haskell compiled 550ms  
Haskell v2 20-30ms  
SML 360ms  
SML v2 10-12ms    
C as benchmark 10ms    

##### Problem no 36 

Prob: find the sum of all numbers that are palindrome in both decimal and binary bases  

Clojure 45-54ms  
SBCL 4,700 ms  
SBCL v2 3ms   
Haskell 770ms    
SML 860ms  
SML v2 ~1ms  

##### Problem no 37

SBCL 1,580ms  
SBCL v2 ~1ms  
SBCL proto-memoize 0.4ms  
Clojure 1,500ms    
Clojure v2 1.3-2.5ms  
Haskell 720ms    
SML ~1ms  

##### Problem 38

SBCL 0.1ms  
Clojure 1-2ms  
Haskell < 10ms  
SML < 1ms  

##### Problem no 39

SBCL 8ms  
SBCL v2 0.3ms  
Clojure 12ms  
Clojure v2 2-3ms  
Haskell 10ms  
Haskell bruteforce 40ms  

##### Problem no 40

SBCL tricky version 0.2ms  
C benchmark tricky version 0.045ms  
Java benchmark tricky version 0.054ms  
Haskell 10ms  
Clojure 31ms  

##### Problem no 41

Problem : The sum of all pandigital primes  

Haskell (one liner) 10ms  
Clojure 2ms (fast)  
Common Lisp 0.15ms (Uberfast!)  

##### Problem 43 

Substring divisibility 

SBCL 0.5ms  

##### Problem 44

Laziness stuff

Clojure (using pmap) 0.21 sec  
Haskell (optimized to the max) 0.23 sec  

##### Problem 45

Clojure 0.1ms  
C as benchmark 0.3ms  

##### Problem 47 

Clojure 180ms  
SBCL 98ms  
C as benchmark 58ms  

##### Problem 48

Clojure 53ms  
SBCL 14ms  

##### Problem 49

SBCL 0.3 ms  

##### Problen no 51

Haskell 320ms  

##### Problem 62

Clojure 43ms  
SBCL 820ms  

##### Problem 63

Prob : How many a^m have m digits?  

SBCL 0.1 ms  
Clojure 0.4 ms  

##### Problem 65  

SBCL 0.3ms  

##### Problem 67

Clojure 1.7 15ms  
Haskell 26ms  
Julia 4ms  

##### Problem 75

SBCL 0.2second  

##### Problem no 76 

Problem : How many ways 100 can be written as the sum of positive integers

Clojure 0.05ms (memoization)  
SBCL 5600ms  
Haskell 2,300ms  

##### Problem 77

Problem : which number can be express as the summation of primes in 5000 different ways

SBCL 4ms  
Clojure 6ms  

##### Problem 80

SBCL 223ms  

##### Problem 82 

Clojure 1.7 8ms  


##### Problem no 85

Clojure 1.5 sec  
SBCL 0.53 sec  

##### Problem 90

Clojure 3secs  

##### Problem 93

Clojure 500ms  

##### Problem 98 

Clojure 300ms  

##### Problem no 100

Clojure 0.039 msec  
SBCL 1.2 msec  

##### Problem 103

Clojure 47ms  

##### Problem 111

Clojure 480ms  

##### Problem 113

Clojure 12ms  

##### Problem 116  

Clojure 0.6ms  
SBCL 0.1ms  

##### Problem 118

Clojure 14secs  

##### Problem no 125

Clojure 0.5 sec  
SBCL 0.6-0.8 sec  

##### Problem no 131

SBCL 3ms  
Haskell interpreted 230ms  

##### Problem 148

Clojure 0.1ms  

##### Problem 164

Clojure 0.3ms  

##### Problem no 173 

Clojure 0.163 sec  
Haskell 8.3 sec  

##### Problem no 174

Clojure 1.2 sec  

##### Problem 188

Clojure 1.7ms  

##### Problem 191

Clojure 11ms  

##### Problem 204

Haskell 890ms  

##### Problem 211

Racket 393secs    

##### Problem no 234

Clojure 2.3 sec  
SBCL 1.4 sec  

##### Problem 297

Clojure 39ms  

##### Problem 315

Clojure 12secs

##### Problem 345

Clojure 1.1sec


##### Problem 346

Clojure 2.6sec  

##### Problem 347

Clojure 1.2sec 

##### Problem 348 

Clojure 10secs 

##### Problem 381  

Clojure 23 secs  

##### Problem 387

Clojure 2 secs 

##### Problem 401

Clojure 33 secs  
Haskell 26 secs  

##### Problem 491 

Clojure 2 secs   

##### Problem 493 

Clojure TBD



## License

Copyright © 2014 PT Zenius Education

Distributed under the Eclipse Public License either version 1.0.






