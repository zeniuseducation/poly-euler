# Polyglottic Euler in 3 Lambdas

Personal polyglottic attempts to euler in Clojure, CLisp, and
Haskell. I will refactor from time to time without deleting old codes. 

## Usage

You can run lein autoexpects for clojure.

In every source, I will list the time-elapsed for each solution. 

## Performance Comparison in my MBA i5 1.6GHz

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

##### Problem no 25

Prob: Find the first fibo element that reach 1000 digits.

Clojure averaging 2.9 msecs  
Clisp averaging 1.3 msecs  
Haskell averaging 20 msecs  

Note: I need to learn more about optimizing haskell codes.

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
