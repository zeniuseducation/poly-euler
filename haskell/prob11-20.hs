import Math
import Data.List

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

pascalRow :: Num a => [a] -> [a]
pascalRow res = [1] ++ zipResult ++ [1]
  where zipResult = zipWith (+) res (tail res)

pascal :: Int -> Int -> [a] -> [a]
pascal n i res
  | n == i = res
  | otherwise = pascal n (succ i) (pascalRow res)

sqr rs = rs * rs

euler15 :: Int -> Int
euler15 n = sum $ map sqr (pascal n 1 [1,1])


