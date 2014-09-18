module Exp where


-- Some experiments

primes = 2 : filter (null . tail . primeFactors) [3,5..]
 
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n = [n]
        | rem n p == 0 = p : factor (quot n p) (p:ps)
        | otherwise = factor n ps
 
problem_3 = last (primeFactors 600851475143)
