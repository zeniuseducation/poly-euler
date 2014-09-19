import Math
import Data.List
import Data.Ord

-- PROBLEM NO 12

-- it returns the n-th triangle number
triangle :: Int -> Int
triangle n = quot (n * (n + 1)) 2

-- it returns the number of factors the n-th triangle number has
triFactors :: Int -> Int
triFactors n = length $ factors $ triangle n

solution12Helper :: Int -> Int -> Int
solution12Helper n i
  | n <= triFactors i = triangle i
  | otherwise = solution12Helper n (succ i)

-- it returns the first triangle number that has n or more number of factors
euler12 :: Int -> Int
euler12 n = solution12Helper n 1

-- PROBLEM NO 14

-- it returns the next element in collatz sequence immediately after n
collatz :: Int -> Int
collatz n
  | even n = div n 2
  | otherwise = succ (3 * n)

-- it returns the collatz sequence starting from i
collatzSeq :: Int -> [Int]
collatzSeq 1 = []
collatzSeq i = res: (collatzSeq res)
  where res = collatz i

countCollatz :: Int -> Int
countCollatz i = length $ collatzSeq i

sol14 :: Int -> Int
sol14 lim = countCollatz $ maximumBy (comparing countCollatz) [n | n <- [2.. (pred lim)]]

-- PROBLEM NO 15

pascalRow :: Num a => [a] -> [a]
pascalRow res = [1] ++ zipResult ++ [1]
  where zipResult = zipWith (+) res (tail res)

pascal :: Num a => Int -> Int -> [a] -> [a]
pascal n i res
  | n == i = res
  | otherwise = pascal n (succ i) (pascalRow res)

sqr rs = rs * rs

euler15 :: Int -> Int
euler15 n = sum $ map sqr (pascal n 1 [1,1])

-- Elapsed time 85 seconds!???

-- problem no 20 : it returns the sum of digits of n!
sol_20 n = sum . numcol . product $ [1..n]







