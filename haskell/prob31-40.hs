import Data.List
import Math


-- PROBLEM 31 NOT SOLVED

coins = [200,100,50,20,10,5,2,1]

step_31a n (x:xs) res
  | n == sum res = res
  | x == n = x : res
  | x > n = step_31a n xs []
  | otherwise = x : step_31a (n - x) (x:xs) (x:res)

-- PROBLEM 34

fact :: Int -> Int
fact 0 = 1
fact n = product [1..n]

sol_34 lim = [x| x <- [10..lim], x == (sum $ map fact (numcol (toInteger x)))]


