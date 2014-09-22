import Math
import Data.List

psqr :: Int -> Bool
psqr n = floor x == ceiling x
  where x = sqrt . fromIntegral $ n

perfects = [[a,b,c]| c <- [1..], b <- [(succ c)..], a <- [(succ b)..],
            psqr (a+b) && psqr (a-b) && psqr (a+c) && psqr (a-c) && psqr (b+c) && psqr (b-c)]


-- Problem 131

findPrimes i n lim res
  | llim > lim = res
  | n < 0 = findPrimes (succ i) i lim res
  | p > lim = findPrimes (succ i) i lim res
  | prime p = findPrimes (succ i) i lim (p:res)
  | otherwise = findPrimes i (pred n) lim res
  where llim = i^3 - (pred i)^3
        p = i^3 - n^3

sol_131 lim = length $ findPrimes 1 0 lim []

-- Problem 134

