-- PROBLEM NO 1

euler1 a b lim = sum [x | x <- [1..lim],
                      (0 == (rem x a)) || (0 == (rem x b))]

-- *Main> euler1 3 5 999
-- it :: Integral a => a
-- (0.01 secs, 2614216 bytes)

-- PROBLEM NO 2

euler2 (a:b:xs) lim res
  | a >= lim = res
  | otherwise = euler2 ((a+b):a: []) lim (if (even a) then (a + res) else res)
                
-- PROBLEM NO 3

