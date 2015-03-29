module Two where

import Data.List
import Data.Time

isort (x:[]) = [x]
isort (x:xs) = smaller ++ [x] ++ larger
  where res = isort xs
        smaller = takeWhile (<= x) res
        larger = dropWhile (<= x) res


factmod _ 0 = 1
factmod _ 1 = 1
factmod m x = rem (x * (factmod m (x-1))) m

factmods m = (!!) (map (factmod m) [0..])

factorial 0 = 1
factorial 1 = 1
factorial n = n * (factorial $ n - 1)

factorials = (!!) (map factorial [0..])

modex a 0 m = 1
modex a 1 m = rem a m
modex a b m =
  let half = modex a (div b 2) m
  in if even b then rem (half * half) m else rem (half * half * a) m

sol458 tar =
  let sisa = product $ take (rem tar 7) [7,6..]
      modi = 10^9
      hasil = modex 7 tar modi
      perm = rem ((modex (factorials 7) (div tar 7) modi) * sisa) modi
  in hasil - perm

prime :: Int -> Bool
prime n = loopi 3
  where loopi :: Int -> Bool
        loopi i
          | i*i > n = True
          | 0 == rem n i = False
          | otherwise = loopi $ i + 2

primes = 2:filter prime [3,5..]

insertBy' f x xs = smaller ++ [x] ++ larger
  where smaller = takeWhile (\k -> f k <= f x) xs
        larger = dropWhile (\k -> f k <= f x) xs

modi = 500500507

finalise :: [(Int,Int)] -> Int -> Int
finalise lst target = loopi lst 1 0
  where loopi ((b,a) :xs) res ctr
          | ctr >= target = res
          | otherwise = loopi xs (rem (res * b) modi) (ctr + a)

minDivisor :: Int -> Int
minDivisor target = loopi raws 0 []
  where raws = map (\x -> (x, 1)) primes
        loopi :: [(Int,Int)] -> Int -> [(Int,Int)] -> Int
        loopi prs counter res
          | counter >= target = finalise res target
          | otherwise = loopi (insertBy' fst newOne (tail prs)) (succ counter) (res++ [head prs])
          where newOne = let (a,b) = head prs in (a^2,b+1)

sol500 :: Int -> Int -> Int -> Int
sol500 target maxi howmany = foldl (\x y -> rem (x*y) modi) 1 results
  where results = take target $ sort total
        total = concat [powers,take target primes]
        powers = concatMap (\x -> takeWhile (< maxi) $ tail $ iterate square x) (take howmany primes)
        square i = i * i

time f x i howmany = do
  start <- getCurrentTime
  print $ f x i howmany
  stop <- getCurrentTime
  print $ diffUTCTime stop start


