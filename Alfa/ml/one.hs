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


sol45 :: Int -> Int
sol45 n = (take n $ filter pentalp hexas) !! (n-1)

primes = 2:filter oddPrime [3,5..]

sol69 lim = takeWhile (< lim) $ scanl1 (*) primes

sol50 :: Int -> Int
sol50 lim = iter (filter (\ (a,b) -> primep b) tmp) 0 0
  where tmp = concatMap (zip [1..]) $
              map (\x -> takeWhile (< lim) $ scanl1 (+) x) $
              takeWhile (not.null) $
              iterate tail $
              takeWhile (< div lim 100) primes
        iter [] i x = x
        iter ((xi,xx) :xs) i x = if xi > i then iter xs xi xx else iter xs i x

triPascal :: Integral a => a -> [[a]]
triPascal n = iter 0 [1] [[1]]
  where iter i cur res
          | i == n = reverse res
          | otherwise = iter (succ i) tmp (tmp:res)
          where tmp = zipWith (+) fil (reverse fil)
                fil = 0:cur

sol53 :: Integral a => a -> Int  
sol53 n = sum [length xs | lst <- triPascal n, let xs = filter (> 1000000) lst]

sol56 lim = maximum [digSum (a^b) | a <- [2..lim], b <- [2..lim]]

time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start
