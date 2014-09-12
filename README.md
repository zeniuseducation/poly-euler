# Polyglottic Euler in 3 Lambdas

Polyglottic attempts to euler in Clojure, Common Lisp (SBCL), and
Haskell. The codes will be refactored from time to time without 
deleting old codes. 

<img src="https://sites.google.com/site/adidozsa2/KeepLisping.jpg">

Note: The logo was taken from <a href="https://sites.google.com/site/adidozsa2/lisp">This blog</a>.

## About the Lisps

"*Lisp is worth learning for the profound enlightenment experience you
will have when you finally get it; that experience will make you a
better programmer for the rest of your days, even if you never
actually use Lisp itself a lot*." - Eric Raymond, "How to Become a
Hacker"

In every source code, time-elapsed for each solution will be listed.

## Performance Comparison in MBA i5 1.6GHz

##### Problem no 1

Prob: Find the sum of all multiples of 3 or 5 that less than 1000

Clojure averaging 3.2 msecs  
SBCL averaging 0.3 msecs  
Haskell averaging 10 msecs  

##### Problem no 2

Prob: Find the sum of even-valued fibo numbers less than 4,000,000

Clojure 0.08msecs  
SBCL less than 0.01msecs  
Haskell ~10msecs  

##### Problem no 3

Prob: Find the largest prime factor of 600851475143

Clojure 32 msecs  
SBCL 37-42 msecs  
Haskell 1,420 msecs  

##### Problem no 4

Prob: Find the largest palindrom that is a product of two 3 digits
number (can be different numbers).

Clojure 16-18 msecs  
SBCL 11-20 msecs  
Haskell 30 msecs  

##### Problem no 5

Prob: Find the smallest number that can be evenly divided by all
integers from 1 to 20.

Clojure 0.3 msecs  
SBCL 0.04 msecs  
Haskel ~10msecs  

##### Problem no 6

Prob: Calculate the difference between the sum of squares of the first 100 natural numbers and the square of sum those numbers.

Clojure 0.3-0.4 msecs  
SBCL 0.01 msecs  
Haskell (it seems that GHCi cannot time under 10msecs)

##### Problem no 25

Prob: Find the first fibo element that reach 1000 digits.

Clojure averaging 2.9 msecs  
SBCL averaging 1.3 msecs  
Haskell averaging 20 msecs  

Note: I need to learn more about optimizing haskell codes.

## License

Copyright © 2014 PT Zenius Education

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.




