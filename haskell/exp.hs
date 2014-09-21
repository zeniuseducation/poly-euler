module Exp where

import Data.List

-- yeah... just to simplify things
div' a b = (0 == rem a b)

-- it returns a list of primes less than lim
sieve lim = takeWhile (< lim) (2 : sieves [3,5..])

-- the helper for sieve
sieves (x:xs) = x : deleteBy (\x n -> div' n x) x (sieves xs)

primesHelper lim (r:rs) (x:xs)
  | x > lim = xs
  | x > (r * r) = primesHelper lim (x:xs) (x:xs)
  | otherwise = [1,1]

primesHelper1 lim (x:xs)
  | x > lim = dropWhile (> lim) xs
  | otherwise = primesHelper1 lim ((reverse nextPrimes) ++ (x:xs))
  where tmplim = if (x * x) > lim then 100+lim else (x * x)
        nextPrimes = removes 0 (map (\p -> primesStep p (reverse (x:xs))) [x+2,x+4.. tmplim])

removes x ls = filter (\i -> i /= x) ls

primesStep :: Int -> [Int] -> Int
primesStep p ls = if null [x| x <- (takeWhile (<= tmp) ls), (div' p x)] then p else 0
  where tmp = ceiling (sqrt (fromIntegral p))

primes lim = 2 : (reverse (primesHelper1 lim [5,3]))

primes' = 2 : filter (null . tail . primeFactors') [3,5..]
 
primeFactors' n = factor n primes'
  where
    factor n (p:ps) 
        | p*p > n = [n]
        | rem n p == 0 = p : factor (quot n p) (p:ps)
        | otherwise = factor n ps
 
primesUnder' lim = takeWhile (< lim) primes'






