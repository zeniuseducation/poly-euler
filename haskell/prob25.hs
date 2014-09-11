euler25 lim (a:b:rest) i
  | a >= lim = i
  | otherwise = euler25 lim ((a+b):a:[]) (succ i)

-- *Main> euler25 (10^999) [1,1] 2
-- it :: (Num a1, Enum a1) => a1
-- (0.02 secs, 6870128 bytes)
