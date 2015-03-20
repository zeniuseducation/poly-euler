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

time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start
