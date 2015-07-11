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

oddprime :: Integral a => a -> Bool
oddprime n = let iterPrime i
                   | i * i > n = True
                   | 0 == rem n i = False
                   | otherwise = iterPrime $ i+2
             in iterPrime 3

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

penta n = div (n* (3*n-1)) 2
pentas = map penta [1..]



