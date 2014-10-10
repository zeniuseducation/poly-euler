module Hmath where

import Clojure
import Data.List


-- it returns true if n is evenly divisible by i
div' :: (Integral a) => a -> a -> Bool
div' n i = (0 == (rem n i))

-- it returns true if n is prime
prime' :: (Integral a) => a -> Bool
prime' n
  | n < 10 = elem n [2,3,5,7]
  | even n = False
  | otherwise = all (\x -> not $ div' n x) tmp
  where tmp = takeWhile (\x -> (x^2) <= n) [3,5..]

-- it returns the first prime greater than p
nextPrime :: (Integral a) => a -> a
nextPrime p
  | p < 2 = 2
  | p == 2 = 3
  | even p = if prime' $ succ p then succ p else nextPrime $ succ p
  | otherwise = head $ dropWhile (not.prime') [p+2,p+4..]

-- it generates infitely lazy seq of primes
primes :: (Integral a) => [a]
primes = iterate nextPrime 2

tprime' n = all prime' $ ln ++ rn
  where ncol = numcol n
        ln = map colnum $ take (length ncol) $ iterate init ncol
        rn = map colnum $ take (length ncol) $ iterate tail ncol

sol37 start = sum $ take 11 $ filter tprime' (dropWhile (< start) primes)
