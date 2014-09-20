import Data.List
import Math

-- Problem 173

laminas lim = [y^2-x^2| x <- [1..limx] , y <- [x+2,x+4.. (succ limx)], (y^2) - (x^2) <= lim]
  where limx = quot lim 4

lamina_2 lim = [[y^2-x^2| y <- [x+2,x+4.. (succ limx)], (y^2) - (x^2) <= lim] | x <- [1..limx]]
  where limx = quot lim 4

sol_174 lim = sum $ map length $ lamina_2 lim

laminas_2 x y res lim 
  | tiles > lim = if x > (quot lim 4) then res else laminas_2 (succ x) (x+3) res lim
  | otherwise = laminas_2 x (y + 2) (succ res) lim
  where tiles = (y^2) - (x^2)

sol_174a lim = laminas_2 1 3 0 lim

distinct lst =  map (head.group.sort) lst

lamina_3 lim = [[d^2 + 2*x*d | d <- [2,4..limx], d^2 + 2*x*d <= lim] | x <- [1..limx]]
  where limx = quot lim 4

laminas_4 x d res lim 
  | tiles > lim = if x > (quot lim 4) then res else laminas_4 (succ x) 2 res lim
  | otherwise = laminas_2 x (d + 2) (succ res) lim
  where tiles = d^2 + 2*x*d

sol_174b lim = laminas_2 1 2 0 lim
