module Math where

import Data.List

primeHelper :: Int -> Int -> Bool
primeHelper p i
  | (i*i) > p = True
  | 0 == (rem p i) = False
  | otherwise = primeHelper p (i + 2)

-- it returns true if p is prime and false otherwise
prime :: Int -> Bool
prime p
  | p <= 10 = elem p [2,3,5,7]
  | even p = False
  | otherwise = primeHelper p 3

factorsHelper :: Int -> Int -> [Int] -> [Int]
factorsHelper n i res
  | i * i > n = res
  | 0 == rem n i = factorsHelper n (succ i) newRes
  | otherwise = factorsHelper n (succ i) res
  where newRes = if i == (quot n i) then i:res else i:(quot n i):res

-- it returns the list of integer factors of n
factors :: Int -> [Int]
factors 1 = [1]
factors 2 = [1,2]
factors 3 = [1,3]
factors 6 = [1,2,3,6]
factors 12 = [1,2,3,4,6,12]
factors n = factorsHelper n 1 []


-- it returns true if p is a palindrom
isPalin :: Int -> Bool
isPalin p = (show p) == (reverse $ show p)

-- it returns the list of prime factors which multiplied to produce
-- lcm of all numbers in the list
rudeLCM :: [Int] -> [Int] -> [Int]
rudeLCM (a:xs) res
  | null xs = a:res
  | any (\x -> 0 == (rem x a)) xs = rudeLCM newXs newRes
  | otherwise = rudeLCM xs (a:res)
  where newXs = map (\x -> (if (0 == (rem x a)) then (quot x a) else x)) xs
        newRes = if (prime a) then a:res else res

-- it returns the first positive prime number greater than x
nextPrime :: Int -> Int
nextPrime x
  | x == 2 = 3
  | even x = if prime $ succ x then succ x else nextPrime $ succ x
  | otherwise = if prime $ 2 + x then 2 + x else nextPrime $ 2 + x

primeListHelper :: Int -> Int -> Int -> [Int] -> [Int]
primeListHelper n i cur res
  | n == i = cur:res
  | otherwise = primeListHelper n (succ i) (nextPrime cur) (cur:res)

-- it returns n first positive prime numbers
primeList :: Int -> [Int]
primeList n = primeListHelper n 1 2 []


sumaPrimaHelper :: Int -> Int -> Int -> Int -> Int
sumaPrimaHelper n i cur res
  | n == i = cur + res
  | otherwise = sumaPrimaHelper n (succ i) (nextPrime cur) (cur+res)

-- it returns the sum of n first positive prime numbers
sumaPrima :: Int -> Int
sumaPrima n = sumaPrimaHelper n 1 2 0

primesUnder :: Int -> [Int]
primesUnder n = takeWhile (< n) $ iterate nextPrime 2 

sumPrimesHelper :: Int -> Int -> Int -> Int
sumPrimesHelper n i res
  | i > n = res
  | otherwise = sumPrimesHelper n (nextPrime i) (i + res)

-- Returns true if n is a perfect square

psquare :: Int -> Bool
psquare n = ceiling x == floor x
  where x = sqrt.fromIntegral $ n

-- it returns the sum of all primes less than n
sumPrimes :: Int -> Int
sumPrimes n = sumPrimesHelper n 2 0

-- it returns the list of digits of a number n
numcol :: Integer -> [Int]
numcol n = if n < 10
           then [fromInteger n]
           else (numcol $ quot n 10) ++ [rem (fromInteger n) 10]

-- it returns a number from a list of digits
colnum :: [Int] -> Integer
colnum ls
  | null ls = 0
  | otherwise = (10 * (toInteger (colnum $ init ls))) + (toInteger $ last ls)

-- yeah... just to simplify things
div' a b = (0 == rem a b)

-- it returns a list of primes less than lim
sieve lim = takeWhile (< lim) (2 : primes [3,5..])

-- the helper for sieve
primes (x:xs) = x : deleteBy (\x n -> div' n x) x (primes xs)

distinct lst =  map head.group.sort $ lst








               


