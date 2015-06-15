module Two where

import Data.List
import Data.Time


fak 0 = 1
fak i = i * (fak $ i - 1)

fact i
  | i == 0 = 1
  | otherwise = i * (fact $ i - 1)

sum' [] = 0
sum' (x : []) = x
sum' (x : xs) = x + sum xs

range i j
  | i == j = [i]
  | otherwise =  i : (range (succ i) j)

prime' x
  | x < 2 = False
  | x == 2 = True
  | even x = False
  | otherwise = loopi 3
  where loopi i
          | i*i > x = True
          | 0 == rem x i = False
          | otherwise = loopi $ i + 2

primes = 2 : filter prime' [3,5..]
prime = (!!) primes

fibo = 1:2:zipWith (+) fibo (tail fibo)

sol2 lim = sum $ filter even $ takeWhile (lim > ) fibo

oddPrime :: Int -> Bool
oddPrime n = iter 3
  where iter i
          | i*i > n = True
          | 0 == rem n i = False
          | otherwise = iter (i+2)

sol3 :: Int -> Int
sol3 tar = iter 3 tar
  where iter i res
          | res == i = i
          | oddPrime i = if 0 == rem res i
                         then iter i (div res i)
                         else iter (i+2) res
          | otherwise = iter (i+2) res

numcol :: Int -> [Int]
numcol n
  | n < 10 = [n]
  | otherwise = numcol (div n 10) ++ [rem n 10]

isPalin :: Int -> Bool
isPalin n = xs == reverse xs
  where xs = numcol n

sol4 :: Int -> Int
sol4 lim = maximum [ x | i <- [lim..999], j <- [lim..999], let x = i*j, isPalin x]

sol5 :: [Int] -> Int
sol5 xs = iter [] xs
  where iter [] (l : ls)  = iter [l] ls
        iter l1 [] = product l1
        iter l1 (l:ls) = iter (l1 ++ [loko]) ls
          where loko = iterone l1 l
                  where iterone [] l = l
                        iterone (lx:lxs) i
                          | 0 == rem i lx = iterone lxs (div i lx)
                          | otherwise = iterone lxs i

time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start
