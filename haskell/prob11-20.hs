import Math
import Data.List

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
