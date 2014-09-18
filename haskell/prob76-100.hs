import Math
import Data.List
import Data.Function

-- PROBLEM 85

csrec i j m n = (succ (m - i)) * (succ (n - j))

crec m n = sum [csrec i j m n | i <- [1..m], j <- [1..n]]

sol_85 size lim = minimumBy (compare `on` snd) res
  where res = [(i*j, abs (lim - (crec i j))) | i <- [1..size], j <- [1..size]]

-- ELAPSED TIME 30 SECONDS!!
