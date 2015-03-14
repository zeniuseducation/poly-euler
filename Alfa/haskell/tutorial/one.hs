module One where

import Data.List
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
 
sieveUO :: Int -> UArray Int Bool
sieveUO top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i 
      when isPrime $ do                   
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
          writeArray sieve j False
    return sieve
 
primesToUO :: Int -> [Int]
primesToUO top | top > 1   = 2 : [2*i + 1 | (i,True) <- assocs $ sieveUO top]
               | otherwise = []

square :: Int -> Int
square x = x * x

isPrime n = let loopi i = if i*i > n
                          then True
                          else if 0 == rem n i
                               then False
                               else loopi (i + 2)
            in loopi 3

isPrime' n = loopi 3
  where loopi i
          | i*i > n = True
          | 0 == rem n i = False
          | otherwise = loopi (i + 2)

fibo x = (lfibo (x-2))+ (lfibo (x-3))

fibos = 1:2:map fibo [3..]

lfibo = (!!) fibos

primes :: [Int]
primes = 2:filter prime' [3,5..]

nPrime :: Int -> Int
nPrime = (!!) primes

prime' :: Int -> Bool
prime' x = all (\i -> 0 /= rem x i) $ takeWhile (\k -> k*k <= x) primes

primesTo :: Int -> [Int]
primesTo lim = takeWhile (< lim) primes


take' n xs
  | n == 0 = []
  | null xs = xs
  | otherwise = (head xs) : take' (n-1) (tail xs)

ptake :: Int -> [a] -> [a]
ptake 0 _  = []
ptake _ [] = []
ptake n (x:xs) = x : ptake (n-1) xs

sum' xs
  | null xs = 0
  | otherwise = (head xs) + (sum' $ tail xs)

psum [] = 0
psum (x:xs) = (+) x $ psum xs

sieves :: [Int]
sieves = [3..]

nlattice :: Int -> Int
nlattice n = if even n then div n 2 else 3*n + 1

lattice :: Int -> Int
lattice 1 = 1
lattice n = succ $ llat $ nlattice n

llat = lattice

welldone lim = maximumBy (\x y -> compare (llat x) (llat y)) [800000..1000000]





