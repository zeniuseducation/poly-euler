module Math where


primeHelper :: Integer -> Integer -> Integer -> Bool
primeHelper p i lim
  | i >= lim = True
  | 0 == (rem p i) = False
  | otherwise = primeHelper p (i + 2) lim

prime :: Integer -> Bool
prime p
  | p <= 20 = elem p [2,3,5,7,11,13,17,19]
  | even p = False
  | otherwise = primeHelper p 3 (ceiling (sqrt (fromInteger p)))

factorsHelper n i lim res
  | i >= lim = res
  | 0 == (rem n i) = factorsHelper n (succ i) lim (i:(quot n i):res)
  | otherwise = factorsHelper n (succ i) lim res

factors n = factorsHelper n 2 (ceiling (sqrt (fromInteger n))) []

isPalin p = (show p) == (reverse $ show p)

rudeLCM (a:xs) res
  | null xs = a:res
  | any (\x -> 0 == (rem x a)) xs = rudeLCM newXs newRes
  | otherwise = rudeLCM xs (a:res)
  where newXs = map (\x -> (if (0 == (rem x a)) then (quot x a) else x)) xs
        newRes = if (prime a) then a:res else res


nextPrime x
  | x == 2 = 3
  | even x = if prime $ succ x then succ x else nextPrime $ succ x
  | otherwise = if prime $ 2 + x then 2 + x else nextPrime $ 2 + x

                                                 

