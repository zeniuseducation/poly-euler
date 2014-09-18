module Math where

primeHelper :: Int -> Int -> Int -> Bool
primeHelper p i lim
  | i >= lim = True
  | 0 == (rem p i) = False
  | otherwise = primeHelper p (i + 2) lim

-- it returns true if p is prime and false otherwise
prime :: Int -> Bool
prime p
  | p <= 10 = elem p [2,3,5,7]
  | even p = False
  | otherwise = primeHelper p 3 (ceiling $ sqrt $ fromIntegral p)

factorsHelper :: Int -> Int -> Int -> [Int] -> [Int]
factorsHelper n i lim res
  | i > lim = res
  | 0 == (rem n i) = factorsHelper n (succ i) lim newRes
  | otherwise = factorsHelper n (succ i) lim res
  where newRes = if i == (quot n i) then i:res else i:(quot n i):res

-- it returns the list of integer factors of n
factors :: Int -> [Int]
factors 1 = [1]
factors 2 = [1,2]
factors 3 = [1,3]
factors 6 = [1,2,3,6]
factors 12 = [1,2,3,4,6,12]
factors n = factorsHelper n 1 (ceiling $ sqrt $ fromIntegral n) []

-- returns true if p is a palindrom
isPalin :: Int -> Bool
isPalin p = (show p) == (reverse $ show p)

rudeLCM :: [Int] -> [Int] -> [Int]
rudeLCM (a:xs) res
  | null xs = a:res
  | any (\x -> 0 == (rem x a)) xs = rudeLCM newXs newRes
  | otherwise = rudeLCM xs (a:res)
  where newXs = map (\x -> (if (0 == (rem x a)) then (quot x a) else x)) xs
        newRes = if (prime a) then a:res else res

nextPrime :: Int -> Int
nextPrime x
  | x == 2 = 3
  | even x = if prime $ succ x then succ x else nextPrime $ succ x
  | otherwise = if prime $ 2 + x then 2 + x else nextPrime $ 2 + x

primeListHelper :: Int -> Int -> Int -> [Int] -> [Int]
primeListHelper n i cur res
  | n == i = cur:res
  | otherwise = primeListHelper n (succ i) (nextPrime cur) (cur:res)

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

sumPrimesHelper n i res
  | i > n = res
  | otherwise = sumPrimesHelper n (nextPrime i) (i + res)

-- it returns the sum of all primes less than n
sumPrimes :: Int -> Int
sumPrimes n = sumPrimesHelper n 2 0










               
