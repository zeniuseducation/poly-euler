module One where

import Data.List

numcol n
  | n < 10 = [n]
  | otherwise = (numcol $ div n 10) ++ [rem n 10]

colnum ls = helper ls 0
  where helper (x:xs) res
          | null xs = x + (10*res)
          | otherwise = helper xs (x+ 10*res)

triangles = map (\n -> div (n * (succ n)) 2) [1..]
squares = map (\x -> x*x) [1..]
pentas = map (\n -> div (n * (pred (3*n))) 2) [1..]
hexas = map (\n -> n * (pred $ 2*n)) [1..]
heptas = map (\n -> div (n * (5*n - 3)) 2) [1..]
octas = map (\n -> n * (3*n - 2)) [1..]

figurates ls = takeWhile (< 10000) $ dropWhile (< 1000) ls
