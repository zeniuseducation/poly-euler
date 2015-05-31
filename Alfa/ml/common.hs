module Common (isprime, oddprime) where

import Data.List

isprime :: Int -> Bool
isprime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = let iterPrime :: Int -> Bool
                    iterPrime i
                      | i*i > n = True
                      | 0 == rem n i = False
                      | otherwise = iterPrime $ i+2
                in iterPrime 3

oddprime :: Int -> Bool
oddprime n = let iterPrime :: Int -> Bool
                 iterPrime i
                   | i * i > n = True
                   | 0 == rem n i = False
                   | otherwise = iterPrime $ i+2
             in iterPrime 3

                   




