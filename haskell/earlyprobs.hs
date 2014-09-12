-- PROBLEM NO 1

euler1 :: Int -> Int -> Int -> Int
euler1 a b lim = sum [x | x <- [1..lim],
                      (0 == (rem x a)) || (0 == (rem x b))]

-- *Main> euler1 3 5 999
-- it :: Integer a => a
-- (0.01 secs, 2614216 bytes)

-- PROBLEM NO 2

euler2 :: [Int] -> Int -> Int -> Int
euler2 (a:b:xs) lim res
  | a >= lim = res
  | otherwise = euler2 ((a+b):a: []) lim (if (even a) then (a + res) else res)
                
-- PROBLEM NO 3

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

euler3 n = maximum $ filter prime (factors n)

;; elapsed time 1.43 seconds







