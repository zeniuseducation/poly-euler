module One where

import Data.List
import qualified Data.Map as M
import qualified Data.Vector as V
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

sum_sieve :: Int -> Int
sum_sieve lim = outer 3 2 refs
  where refs = V.replicate (succ lim) True
        llim = ceiling.sqrt.fromIntegral $ lim
        outer i res resi
          | i >= lim = res
          | resi V.! i = if i <= llim
                         then outer (i+2) (i+res) resj
                         else outer (i+2) (i+res) resi
          | otherwise = outer (i+2) res resi
          where j= i*i
                resj = resi V.// (zip [j,j+2*i..lim] $ repeat False)



lsum_sieve :: Int -> Int 
lsum_sieve lim = loopi (M.fromList []) 3 2
  where llim = ceiling $ sqrt $ fromIntegral lim
        loopi refs i res 
          | i <= llim = if convert $ M.lookup i refs
                        then loopi (loopj refs (i*i))  (i + 2) (res+i)
                        else loopi refs (i+2) res
          | i <= lim = if convert $ M.lookup i refs
                       then loopi refs (i+2) (res+i)
                       else loopi refs (i+2) res 
          | otherwise = res 
          where convert x = case x of
                  Just x -> x
                  Nothing -> True
                loopj refj j 
                  | j > lim = refj
                  | otherwise = loopj (M.insert j False refj) $ j + (2*i)

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

sol71 lim = starti
  where (start:_) = dropWhile (\x -> 0 /= rem x 7) [lim,lim-1..]
        starti = 3* (div start 7) - 1

isParty :: Int -> Bool
isParty n = all (\x -> (sort $ numcol x) == tmp) $ map (*n) [2..6]
  where tmp = sort $ numcol n

sol52 :: Int -> Int
sol52 start = head $ dropWhile (not.isParty) $ [start..]

isLychrel :: Integral a => a -> Bool
isLychrel n = b >= 50
  where f (a,b) = (a + (colnum $ reverse $ numcol a), b+1)
        ((a,b):_) = dropWhile (\(a,b) -> (not.isPalin $ a) && b < 50) $ iterate f (f (n,0))

sol55 :: Integral a => a -> Int
sol55 lim = length $ filter isLychrel [1..lim]

corners :: Int -> [Int]
corners i = take 4 $ iterate (\x -> x - (i-1)) (i*i)

sol58 :: Int -> (Int,Int,Int)
sol58 n = head $ dropWhile (\(a,b,c) -> (g a b) >= n) $ iterate f (f (1,0,1))
  where f (a,b,c) = (a+4, (+) b $ length $ filter primep $ corners $ 2+c, c+2)
        g a b = div (100*b) a

jumdig = length.show

sol63 n = sum $ map f [1..n]
  where f i = length $ takeWhile (\x -> x == (jumdig $ i^x)) [1..]

sqrt2 lim = iter lim (1,1)
  where iter i (a,b)
          | i == 1 = radd (a,b) (1,2)
          | otherwise = iter (i-1) $ radd (a,b) (rdiv (1,1) $ radd (2,1) (1,2))

sol73 lim = iter 5 0
  where iter i res
          | i > lim = res
          | otherwise = iter (i+1) (res+resi)
          where start = 1 + (div i 3)
                end = 1 + (div i 2)
                resi = length $ filter (\x -> 1 == gcd i x) $
                       takeWhile (< end) $ [start..]

sol73b lim = sum $ [length $ filter (\i -> 1 == gcd x i) s | x <- [5..lim],
                    let s = takeWhile (< (1+(div x 2))) [(1+(div x 3))..]]

intersperse' a [x] = [x]
intersperse' a (x:xs) = x:a:intersperse' a xs
        
time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start







