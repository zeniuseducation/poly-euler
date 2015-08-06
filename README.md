# Polyglottic Euler in 5 Lambdas

Polyglottic attempts to euler in Clojure, Common Lisp (SBCL), Racket, Julia, SML and
Haskell. The codes will be refactored from time to time without 
deleting old codes. 

<img src="https://sites.google.com/site/adidozsa2/KeepLisping.jpg">

Note: The logo was taken from <a href="https://sites.google.com/site/adidozsa2/lisp">This blog</a>. 
Unfortunately nobody noticed that the Johnnie posture is lambda-shaped :)

### The Eulerians:  
<img src="https://projecteuler.net/profile/squest.png">
<img src="https://projecteuler.net/profile/skadinyo.png">
<img src="https://projecteuler.net/profile/memeri.png">
<img src="https://projecteuler.net/profile/soel.png">
<img src="https://projecteuler.net/profile/calvin91.png">
<img src="https://projecteuler.net/profile/soeharto.png">


## About the Lisps

"*Lisp is worth learning for the profound enlightenment experience you
will have when you finally get it; that experience will make you a
better programmer for the rest of your days, even if you never
actually use Lisp itself a lot*."   
~ Eric Raymond, "How to Become a Hacker"

## Rationales

1. The need for benchmark for a particular euler problem in terms of speed
2. Encouragement for others to come up with faster/better solutions
3. PE problems are good practice for optimising codes, which is critical skill in the era of big-data
4. Finding interesting ideas for creating problems :)  

In every source code, time-elapsed for each solution will be listed.

## Performance Comparison on MBA i7 1.7GHz

Implementations:  
1. Clojure using Clojure (Mostly 1.7) on JDK 1.8 (Also used for Java)  
2. Clojurescript on NodeJS  
3. Common Lisp using SBCL 1.2.x  
4. Haskell using GHC compiled using -O2 optimisation   
5. SML using SMLNJ & MLTON (take the faster one, although in general MLTON is much faster)  
6. Racket mostly using Racket 6.1.x and Typed Racket  
7. C using whatever there is on Macbook, optimized to the max using -Ofast  
8. Julia using Julia 0.3.x (soon to be upgraded to 0.4)  
9. Python using pypy  
10. F3 using Mono on Mac  

##### Problem no 1

Prob: Find the sum of all multiples of 3 or 5 that less than 1000

Clojure averaging 3.2 msecs  
Clojure 1.7 0.08ms  
SBCL averaging 0.3 msecs  
Haskell 0.07ms
Racket less than 1 msec  
Java benchmark 0.05ms  
C benchmark 0.05ms  
Swift 0.018ms  
Julia 0.008ms NEW RECORD!  
Elixir 0.1ms 
F# less than 1ms  
SML 0.016ms  
Pypy 0.027ms  

##### Problem no 2

Prob: Find the sum of even-valued fibo numbers less than 4,000,000

Clojure 0.08 msecs  
Clojure 1.7 0.04ms  
SBCL less than 0.01 msecs   
Haskell 0.06ms  
Racket less than 1 msec  
C Benchmark 0.03ms  
Java benchmark 0.005ms  
Swift 0.004ms  
Julia 0.004ms TIGHT with SWIFT!  
Pypy 0.02ms  
Elixir 0.008ms  

##### Problem no 3

Prob: Find the largest prime factor of 600851475143

Clojure 32 msecs  
Type hinted Clojure using transient and unchecked-math 5-6msecs  
Further optimised type-hinted Clojure 0.3-0.4ms  
SBCL 37-42 msecs  
Type hinted SBCL 4-6ms  
Further optimised SBCL 0.45ms  
Haskell 0.5ms  
SML 2 ms  
Racket 50-60 ms  
Typed Racket 6ms   
Further optimised Typed Racket 0.4-0.5ms  
C as benchmark 0.2ms  
Java as benchmark 5ms  
Java v2 0.02ms  
Swift 0.11ms NEW RECORD!!  
Julia 0.16ms  
Juliabox-0.4 v2 0.043ms  
Juliabox-0.4 v3 0.019ms    
Pypy 1.5ms  
Elixir 3.5ms  
F# less than 0.5 ms  

##### Problem no 4

Prob: Find the largest palindrom that is a product of two 3 digits
number (can be different numbers).

Clojure 16-18 msecs  
Clojure with memoization 0.01ms  
SBCL 11-20 msecs  
Haskell 4ms  
SML 1.3 msec  
Java 2ms  
Julia 0.006ms  FAST! (using memoization)  
Pypy 5.5ms  
F# 1ms  

##### Problem no 5

Prob: Find the smallest number that can be evenly divided by all
integers from 1 to 20.

Clojure 0.1 ms  
SBCL 0.006 ms   
Haskell 0.07ms    
Julia 0.005ms  
Pypy 0.07ms  
SML 0.006ms  
Java 0.004ms  
Racket less than 0.5ms  
F# less than 0.5ms  

##### Problem no 6

Prob: Calculate the difference between the sum of squares of the first 100 natural numbers and the square of sum those numbers.

Clojure 0.3-0.4 msecs  
SBCL 0.01 msecs  
Haskell 0.1ms  
Julia 0.004ms  
Pypy 0.4ms  
Java 0.005ms  
F# less than 1ms  
SML 0.007ms  


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
Typed Racket using sieve  1-2ms  
Haskell 25ms  
SML 17 msecs  
C Benchmark using sieve 0.9ms  
Swift naive using Int32 10ms  
Swift using sieves 0.7ms  
Java 1.2ms  
Julia using sieves 0.7ms  
Pypy 6ms  
SML 1ms  
F# less than 0.5ms  

##### Problem no 8

Prob: Find the largest product of 13 digits in a 1000 digits series

Clojure 45-65 msecs    
Clojure 1.7 3ms    
SBCL 23 msecs  
Haskell 3ms  
Julia 0.7ms  
F# 3ms  

##### Problem no 9

Type-hinted Clojure 3-4ms  
Further optimised Clojure 0.4ms  
Crazy clojure technique 0.06ms  
Type-hinted SBCL 3ms  
Typed Racket 1ms  
Haskell 13ms  
SML 1 ms  
C as benchmark 0.25ms  
Java benchmark 2.5ms  
Java v2 1.1ms  
Swift 0.32 ms  
Julia 0.006ms  
Pypy 0.03ms  

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
Type-hinted SBCL using sieves 21ms  
Haskell 1,320 msecs (1.3 secs)  
Haskell on Win32 470 msecs   
SML 736 msecs!! (0.736 secs)    
SML sieve 18ms  
Java sieve 6.3ms  
Racket 4-6K msecs  
Typed Racket 2,200 msecs (Faster than Clojure)  
Typed Racket in Win32 987 msecs  
Typed Racket using sieves 29ms  
C Benchmark using sieve 24 ms  
Swift naive 1,421ms  
Swift sieves 16ms  
Julia using sieves 9ms  
Pypy 75ms  
F# 7ms  

##### Problem no 12

Prob: Triangle number that has more than 500 factors

Clojure 1,700 ms    
Type hinted Clojure 560 ms    
Clojure v2 using memoized 13ms  
SBCL 2,400 ms    
Type hinted SBCL 590 ms    
SBCL v2 21 ms    
SBCL v3 sieve-like 8ms  
SBCL using proto-memoize 1ms  
Typed Racket 1000 ms    
Racket v2 42 ms    
Haskell compiled 660 ms      
Haskell v2 30ms  
SML 730ms    
SML v2 9.5 ms    
C Benchmark 580 ms  
C Benchmark v2 6 ms  
C Benchmark proto-memoize 3 ms  
Java Benchmark 11 ms  
Java v2 1.2ms  
Swift 7ms   
Julia 9ms 
Julia v2 0.2ms  
F# 7ms    

##### Problem 13

Clojure 1.3ms  
Haskell 3ms  
Julia 0.3ms  

##### Problem no 14

Problem : Longest collatz sequence that started from n below 1million  

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
SML MLTON 627ms  
C as benchmark (starting from half a million and check odd only) 179ms   
Java 55ms  
Swift 185ms  
Julia 800ms  
Julia v2 39ms  
F# with Dynamic programming 471ms  

##### Problem no 15

Clojure 0.14 msec  
SBCL 0.018 msec  
Haskell 0.08ms  
Julia 0.07ms  
SML 0.012ms  
F# less than 0.5ms   

##### Problem no 16

Clojure 1.3ms  
Julia 0.8ms  
Haskell 0.2ms  
Pypy 2ms  
F# 1ms  

##### Problem 17

Julia 2ms  
Haskell 1ms  

##### Problem 18

Clojure 1ms  
Julia 0.2ms  
Haskell 1ms  
F# less than 0.5ms  

##### Problem 19 

Haskell 1ms  

##### Problem no 20

Clojure 0.7ms  
Julia 0.4ms  
Haskell 0.1ms  
F# less than 0.5ms  

##### Problem no 21

Problem : The sum of all amicable numbers less than 10,000  

Clojure 72 ms    
Type hinted Clojure 17-22 ms  
Clojure 1.7 11ms  
Clojure v2 2.4ms  
SBCL 51 ms    
Type hinted SBCL 17-20 ms  
Haskell compiled 30 ms  
Typed Racket 24ms  
SML 7ms  
Java benchmark 14ms  
Java 2.2ms  
C as benchmark 7ms   
C using dynamic programming 4.5ms   
Julia 7ms  
Juliabox v2 1.3ms  
F# using list 5ms, and 4ms using loop  
F# v2 less than 1ms  

##### Problem 22 

Haskell 90ms  
Julia 23ms  
F# 108ms  

##### Problem 23 

Problem : The sum of all integers that cannot be written as the sum of two abundant numbers  

Type-hinted Clojure 240-250ms  
Clojure 1.7 110ms  
Clojure 1.7 v2 50ms  
Type-hinted SBCL 275ms  
SBCL sieve-like 102ms  
Typed Racket 450-460ms   
Haskell 14 seconds  
C as benchmark 128ms    
Julia 230ms  
Julia v2 on Juliabox-0.4 runs in 25ms  
SML MLTON 132ms  
F# 102ms  
F# v2 27ms  

##### Problem 24

Problem: Very nice problem, highly recommended!!

Clojure 0.23 msec  
SBCL 0.02 msec  
Haskell 1 msec  
Julia 0.07ms  
SML 0.007ms  

##### Problem no 25

Prob: Find the first fibo element that reach 1000 digits.

Clojure averaging 2.9 msecs  
Type hinted Clojure using *unchecked-math* < 1 ms  
SBCL averaging 1.3 msecs   
SBCL One-liner 1ms  
Racket 3-4 msecs  
Typed Racket 1 ms  
Haskell averaging 10 msecs  
Haskell 0.8ms  
SML 36 msecs  (SML LargeInt is not efficient)  
SML MLTON 1.5ms  
Julia 4ms  
F# 1ms  

##### Problem no 26 

Prob: Very nice problem, find the max recurring digits of recyprocal 1/d where d < 1  

Type-hinted Clojure 0.5-0.6ms  
Type-hinted SBCL 1.2ms  
Typed Racket 0.6-0.8ms  
Haskell 11ms  
SML 0.4-0.6ms  
C as benchmark 0.12ms  
Python for benchmark 8.7ms  
Julia 0.07ms  

##### Problem 27

Prob: Pretty complex to write here  

Clojure 27-28ms  
SBCL 36-38ms  
SBCL using proto-memoize 10ms  
Haskell 80ms  
SML 3,290ms (Very sloow)  
C as benchmark 20ms   
Julia 41ms  

##### Problem 28

Prob: Some fun stuffs  

Clojure 0.082ms  
SBCL 0.023ms  
Haskell 0.4ms  
C as benchmark 0.042ms  
Julia 0.03ms  

##### Problem 29

Clojure 0.065 msec  
SBCL 0.019 msec  
Haskell (one liner) 1.2 sec  
SML 2.4 sec (Very inefficient sml)  
Julia 15ms  

##### Problem no 30

Clojure 730ms    
Parallel mapping Clojure 92ms  
Type-hinted SBCL 820ms  
Haskell compiled 320ms  
New version Haskell 170ms  
SML 1,620ms  
SML new version 315ms  
C Benchmark 287ms     
Julia 19ms  

##### Problem 31

Problem : How many ways we can write 200 as the sum of coins 1,2,5,10,20,50,100,200  

Clojure using memoization 7.2ms & 0.1ms for subsequent calls  
Clojure without memoization 3ms  
Racket without memoization 5ms  
Haskell 4ms  
SBCL 4ms  
SML 3ms  
C Benchmark 0.5ms  
Julia 17ms  

##### Problem 32

Prob: axb = c where a++b++c pandigital 1-9  

Clojure 120ms  
Common Lisp 57ms  
Haskell compiled 19 sec  
Haskell v2 ~60ms  
SML 77ms  


##### Problem 33 

SBCL 0.8 msec  
Clojure 18ms  
Julia 17ms  

##### Problem 34

Prob: find the sum of all numbers that are equal to the sum of factorials of their digits  

SBCL 0.76 sec  
Haskell compiled 0.46sec & 5.3 sec depends on limit  
SML 0.349 & 1.349 depends on limit  
Julia 11ms  

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
Julia 17ms  

##### Problem no 36 

Prob: find the sum of all numbers that are palindrome in both decimal and binary bases  

Clojure 45-54ms  
SBCL 4,700 ms  
SBCL v2 3ms   
Haskell 770ms    
Haskell v2 2.6ms  
SML 860ms  
SML v2 ~1ms  
Julia 1.3ms  

##### Problem no 37

SBCL 1,580ms  
SBCL v2 ~1ms  
SBCL proto-memoize 0.4ms  
Clojure 1,500ms    
Clojure v2 1.3-2.5ms  
Haskell 720ms    
Haskell 0.5ms  
SML ~1ms  
Julia 2ms  

##### Problem 38

SBCL 0.1ms  
Clojure 1-2ms  
Haskell 0.2ms  
SML < 1ms  
Julia 0.08ms  

##### Problem no 39

SBCL 8ms  
SBCL v2 0.3ms  
Clojure 12ms  
Clojure v2 2-3ms  
Haskell 10ms  
Haskell bruteforce 40ms  
Julia 22ms  

##### Problem no 40

SBCL tricky version 0.2ms  
C benchmark tricky version 0.045ms  
Java benchmark tricky version 0.054ms  
Haskell 10ms  
Clojure 31ms  
Julia 0.03ms  

##### Problem no 41

Problem : The sum of all pandigital primes  

Haskell (one liner) 10ms  
Clojure 2ms (fast)  
Common Lisp 0.15ms (Uberfast!)  
Julia 1.3ms  

##### Problem 42 

Haskell 10ms  

##### Problem 43 

Substring divisibility 

SBCL 0.5ms  
Julia 1ms  
Haskell 51ms  

##### Problem 44

Laziness stuff

Clojure (using pmap) 0.21 sec  
Haskell (optimized to the max) 0.23 sec  
Julia 41ms  

##### Problem 45

Clojure 0.1ms  
C as benchmark 0.3ms  
Julia 0.7ms  

##### Problem 46 

Clojure 2.5ms  
Haskell 2.5ms  
Julia 22ms  

##### Problem 47 

Clojure 180ms  
SBCL 98ms  
C as benchmark 58ms  
Julia 220ms  

##### Problem 48

Clojure 3.6ms  
Haskell 4.3ms  
SBCL 4ms  
Julia 1.7ms  

##### Problem 49

SBCL 0.3 ms  
Julia 1.5ms  
Haskell 1.4ms  

##### Problen no 51

Haskell 320ms  

##### Problem 52 

Problem : What is the smallest number where n,2n,3n,4n,5n,6n contain the same digit with different order  

Clojure (1.7) 201ms  
Julia 76ms  

##### Problem 53 

Clojure 1.7 (bruteforce) 2ms  
Julia 0.8ms 

##### Problem 55

Julia 28ms  

##### Problem 56 

Julia 11ms  

##### Problem 57

Julia 98ms  

##### Problem 58

Julia 0.4sec

##### Problem 59

Julia 76ms  

##### Problem 62

Clojure 43ms  
SBCL 820ms  
Julia 24ms  
F# 30ms  

##### Problem 63

Prob : How many a^m have m digits?  

SBCL 0.1 ms  
Clojure 0.4 ms  
Julia 0.07ms  

##### Problem 65  

SBCL 0.3ms  
Julia 1.3ms  

##### Problem 67

Clojure 1.7 15ms  
Haskell 26ms  
Julia 4ms  
F# 1ms  


##### Problem 69

Julia 0.1ms  

##### Problem 71  

Julia 0.03ms 
Haskell less than 1ms  

##### Problem 72

Julia 102ms  
F# 53ms  

##### Problem 73 

Julia 1.2sec

##### Problem 75

Problem : How many L <= 1,500,000 that can form a right-triangle only once.

SBCL 0.2second  
Clojure 1.7 61ms  
Julia 47ms  

##### Problem no 76 

Problem : How many ways 100 can be written as the sum of positive integers

Clojure 0.05ms (memoization)  
SBCL 5600ms  
Haskell 2,300ms  
Julia 6ms  

##### Problem 77

Problem : which number can be express as the summation of primes in 5000 different ways

SBCL 4ms  
Clojure 6ms  

##### Problem 80

SBCL 223ms  

##### Problem 82 

Clojure 1.7 8ms  

##### Problem 81 

Julia 8ms  

##### Problem 84

Clojure 600ms  

##### Problem no 85

Clojure 1.5 sec  
SBCL 0.53 sec  
Julia 11ms  

##### Problem 87

Julia 0.72sec  

##### Problem 90

Clojure 3secs  

##### Problem 92 

Julia 98ms  

##### Problem 93

Clojure 500ms  

##### Problem 95

Julia 178ms 

##### Problem 98 

Clojure 300ms  

##### Problem 99

Julia 2ms  

##### Problem no 100

Clojure 0.039 msec  
SBCL 1.2 msec  

##### Problem 101

Racket 6ms (Need to use matrix-solve)  

##### Problem 103

Clojure 47ms  

##### Problem 107

Clojure 30ms  

##### Problem 108

Julia 16ms  

##### Problem 110

Clojure 47ms  

##### Problem 111

Clojure 480ms  

##### Problem 113

Clojure 12ms  
Julia 6ms  

##### Problem 114

Clojure 20ms  

##### Problem 115

Clojure 0.3ms  

##### Problem 116  

Clojure 0.6ms  
SBCL 0.1ms  
Julia 1.6ms  

##### Problem 117

Julia 7ms  

##### Problem 118

Clojure 14secs  

##### Problem 119

Julia 0.4ms  

##### Problem 123

Julia 234ms 

##### Problem no 125

Clojure 0.5 sec  
SBCL 0.6-0.8 sec  

##### Problem no 129

Clojure 52ms  
Julia 35ms  

##### Problem no 130

Clojure 219ms  
Julia 97ms  

##### Problem no 131

SBCL 3ms  
Haskell interpreted 230ms  

##### Problem no 136 

Julia 3.6sec  

##### Problem no 139

Clojure 12sec  
Julia 2sec  

##### Problem 148

Clojure 0.1ms  

##### Problem 150

Clojure 44sec  

##### Problem 159 

Clojure 213ms  
Julia 1.56sec  

##### Problem 164

Clojure 0.3ms  

##### Problem no 169  

Clojure 1ms (memoized)  

##### Problem no 173 

Clojure 0.163 sec  
Clojure loop version 32ms  
Haskell 8.3 sec  
Julia 23ms  
Julia v2 2ms  
F# 2ms  

##### Problem no 174

Clojure 1.2 sec  
Clojure v2 64ms  
Julia 36ms  
Julia-0.4 v2 18ms  
F# 10ms  

##### Problem 179 

Julia 1.8sec  

##### Problem 183

Clojure 67sec  

##### Problem 187

Julia 498ms  
Clojure 1272ms  

##### Problem 188

Clojure 1.7ms  
Julia 6ms  

##### Problem 191

Clojure 11ms  
Julia 0.5ms  

##### Problem 204

Haskell 890ms  

##### Problem 211

Racket 393secs    

##### Problem 215

Clojure 249ms  (once the crack matrix has been built)  

##### Problem no 234

Clojure 2.3 sec  
SBCL 1.4 sec  

##### Problem 249 

Clojure 1001seconds  

##### Problem 286 

Clojure 98ms  

##### Problem 297

Clojure 39ms  

##### Problem 315

Clojure 12secs  

##### Problem 303

Clojure 57sec  

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

##### Problem 429

Clojure 9.3sec  

##### Problem 491 

Clojure 2 secs   

##### Problem 493 

Clojure TBD

##### Problem 500

Julia 192ms  
Haskell 8.5sec  

##### Problem 504 

Julia 28seconds  
Clojure 62seconds  

##### Problem 512

Julia 49seconds  


## License

Copyright © 2014 PT Zenius Education

Distributed under the Eclipse Public License either version 1.0.






