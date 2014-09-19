import Data.List
import Math

euler25 lim (a:b:rest) i
  | a >= lim = i
  | otherwise = euler25 lim ((a+b):a:[]) (succ i)

-- *Main> euler25 (10^999) [1,1] 2
-- it :: (Num a1, Enum a1) => a1
-- (0.02 secs, 6870128 bytes)

-- PROBLEM no 21

--it returns the sum of all proper divisors of n
sumPropDivisors :: Int -> Int
sumPropDivisors n = sum (factors n) - n

-- it returns n if n is an amicable number and 0 if otherwise
amicable :: Int -> Int
amicable n = if n /= pair && n == sumPropDivisors pair then n else 0
  where pair = sumPropDivisors n
        
-- it returns the sum of all amicable numbers not exceeding lim
sol_21 :: Int -> Int
sol_21 lim = sum $ map amicable [2..lim]


-- PROBLEM 24

-- very naive
sol_24a n = (sort $ permutations "0123456789") !! n

step_24 n digs res raw
  | digs == 0  = (res, raw, n)
  | otherwise = step_24 (rem n fak) (pred digs) (dig:res) (delete dig raw)
  where fak = product [1..digs]
        dig = div n fak








