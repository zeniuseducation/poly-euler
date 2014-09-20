import Data.List

-- yeah... just to simplify things
div' a b = (0 == rem a b)

-- it returns a list of primes less than lim
sieve lim = takeWhile (< lim) (2 : sieves [3,5..])

-- the helper for sieve
sieves (x:xs) = x : deleteBy (\x n -> div' n x) x (sieves xs)

primes' = 2 : filter (null . tail . primeFactors') [3,5..]
 
primeFactors' n = factor n primes'
  where
    factor n (p:ps) 
        | p*p > n = [n]
        | rem n p == 0 = p : factor (quot n p) (p:ps)
        | otherwise = factor n ps
 
primesUnder' lim = takeWhile (< lim) primes'

primes lim = siva ++ [x | x <- [xs+2,xs+4..lim], primeStep x siva]
  where siva = primesUnder' (20 + (ceiling (sqrt (fromIntegral lim))))
        xs = last siva

primeStep p (x:xs)
  | (x*x) > p = True
  | (x * x) == p = False
  | div' p x = False
  | otherwise = primeStep p xs
        
