module Common where

import Data.List
import qualified Data.Set as Set

toDigits :: Int -> [Int]
toDigits n = iter n []
  where iter i res
          | i < 10 = i:res
          | otherwise = iter (div i 10) $ (rem i 10) : res

toNumber :: [Int] -> Int
toNumber lst = iter lst 0
  where iter (x: []) res = x + res*10
        iter (x:xs) res = iter xs (x + res*10)

prime' :: Int -> Bool
prime' n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = iter 3
  where iter i
          | i * i > n = True
          | rem n i == 0 = False
          | otherwise = iter (i+2)

primes :: [Int]
primes = 2 : filter prime' [3,5..]

primesUnder :: Int -> [Int]
primesUnder lim = takeWhile (<lim) primes

maxBy f (x: []) = x
maxBy f (x:xs) = let nMax = maxBy f xs
                 in if f x > f nMax then x else nMax

pandig' xs = sort xs == [1..9]

permutate xs n = iter (map (\x -> [x]) xs) 1
  where setXs = Set.fromList xs
        iter res i
          | i == n = res
          | otherwise = iter [r ++ [a] | r <- res ,
                              a <- Set.toList $
                                   Set.difference setXs (Set.fromList r)] (succ i)

nextPrime :: Int -> Int
nextPrime n
  | n < 2 = 2
  | n == 2 = 3
  | otherwise = loopi n
  where loopi i
          | even i = if prime'(i+1) then i+1 else loopi $ succ i
          | prime' (i+2) = i+2
          | otherwise = nextPrime $ i + 2

permutation' :: Int -> Int -> Bool
permutation' a b = (sort (toDigits a)) == (sort (toDigits b))





