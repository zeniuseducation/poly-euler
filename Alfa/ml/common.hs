module Common where

import Data.List

isprime :: Integral a => a  -> Bool
isprime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = let iterPrime i
                      | i*i > n = True
                      | 0 == rem n i = False
                      | otherwise = iterPrime $ i+2
                in iterPrime 3

digSum :: Integral a => a -> a
digSum x = iter x 0
  where iter n res 
          | n < 10 = n + res
          | otherwise = iter (div n 10) (res + rem n 10)

primep :: Int -> Bool
primep 2 = True
primep n
  | n < 2 = False
  | even n = False
  | otherwise = iter 3
  where iter i
          | 0 == rem n i = False
          | i > div n i = True
          | otherwise = iter $ i + 2

oddprime :: Integral a => a -> Bool
oddprime n = let iterPrime i
                   | i * i > n = True
                   | 0 == rem n i = False
                   | otherwise = iterPrime $ i+2
             in iterPrime 3

oddPrime :: Int -> Bool
oddPrime n = iter 3
  where iter i
          | i > div n i = True
          | 0 == rem n i = False
          | otherwise = iter $ i+2

nextOddPrime :: Int -> Int
nextOddPrime n = if n == 2 then 3 else iter 3
  where iter i
          | oddPrime (i+2) = i+2
          | otherwise = iter $ i+2

numcol :: Integral a => a -> [a]
numcol n = iter n []
  where iter i res
          | i < 10 = i:res
          | otherwise = iter (div i 10) $ (rem i 10):res

colnum :: Integral a => [a] -> a
colnum lst = iter lst 1
  where iter [] _ = 0
        iter (x: []) res = (+) x $ res*10
        iter (x:xs) res = iter xs $ (res * 10) + x

fibo = 1:2:zipWith (+) fibo (tail fibo)

intp :: Double -> Bool
intp x = floor x == ceiling x

pentalp :: Integral a => a ->  Bool
pentalp x = intp res
  where res = ((+) 1 (sqrt $ fromIntegral $ 1 + 24* x) / 6.0)

penta :: Int -> Int 
penta n = div (n* (3*n-1)) 2

pentas :: [Int]
pentas = map penta [1..]

hexa :: Int -> Int 
hexa n = n * (2*n-1)

hexas :: [Int]
hexas = map hexa [1..]








