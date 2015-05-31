module One where

import Data.List
import Data.Time
import Data.List.Split
import Common


sumPrimes :: Int -> Int
sumPrimes lim = let iterSum :: Int -> Int -> Int
                    iterSum i res
                      | i > lim = res
                      | oddprime i = iterSum (i+2) (i+res)
                      | otherwise = iterSum (i+2) res
                in iterSum 3 2


time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start
