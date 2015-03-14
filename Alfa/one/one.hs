module One where

import Data.List

prime :: Int -> Bool
prime n = loopi 3
  where loopi :: Int -> Bool
        loopi i
          | i*i > n = True
          | 0 == rem n i = False
          | otherwise = loopi $ i +2

sumPrimes :: Int -> Int
sumPrimes lim = loopi 3 2
  where loopi :: Int -> Int -> Int
        loopi i res =
          if i > lim
          then res
          else loopi (i+2)
               (if prime i then i+res else res)


