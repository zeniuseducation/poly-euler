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

-- reimplementing iterate

iterateHelper :: (a -> a) -> a -> [a]
iterateHelper f x = x : (iterateHelper f (f x))

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : (iterateHelper f (f x)) ++ []

range i j = if i == j then [] else i : (range (succ i) j)

sum' (x:xs) = if null xs then 0 else x + sum' xs

prod' (x:xs) = if null xs then x else x * prod' xs

