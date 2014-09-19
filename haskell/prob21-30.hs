import Data.List
import Math


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


-- PROBLEM 23

abundant' x = x > sumPropDivisors x

nonabundant' x = x <= sumPropDivisors x

-- PROBLEM 24

-- very naive
sol_24a n = (sort $ permutations "0123456789") !! n

step_24 :: Int -> Int -> [Int] -> [Int] -> [Int]
step_24 n digs res raw
  | digs == 0  = res
  | otherwise = step_24 (rem n fak) (pred digs)
                ((raw!!dig) :res) (delete (raw!!dig) raw)
  where fak = product [1..(pred digs)]
        dig = div n fak

sol_24 :: Int -> [Int]
sol_24 n = reverse $ step_24 n 10 [] [0..9]

-- PROBLEM 25

euler25 lim (a:b:rest) i
  | a >= lim = i
  | otherwise = euler25 lim ((a+b):a:[]) (succ i)

-- *Main> euler25 (10^999) [1,1] 2
-- it :: (Num a1, Enum a1) => a1
-- (0.02 secs, 6870128 bytes)

-- PROBLEM 28



-- Problem 29

sol29 n = length $ nub [a^b|a <- [2..n], b <- [2..n]]

-- Problem 30

sol30 n = sum [i| i <- [2..(n * (9^n))], i == sum (map (^n) (numcol (toInteger i)))]





