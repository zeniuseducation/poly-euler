import Data.List
import Math

-- PROBLEM 31 NOT SOLVED

coins = [200,100,50,20,10,5,2,1]
target31 = 200


-- PROBLEM 34


fact 0 = 1
fact n = product [1..n]
facts = map fact [0..9]



raws = distinct $ map sum (subsequences facts)
conds34 i = i == sum (map fact $ intcol i)

sol34 = filter conds34 raws

-- PROBLEM NO 33

colint ls
  | null ls = 0
  | otherwise = (10 * (colint $ init ls)) + last ls


sol_33 n  = [(x,y,z) | x <- [0..9], y <- [0..9], z <- [0..9], conds x y z]

conds x y z = (x/y) == (xz/yz) || (x/y) == (xz/zy) || (x/y) == (zx/zy) || (x/y) == (zx/yz)
  where xz = colint [x,z]
        yz = colint [y,z]
        zx = colint [z,x]
        zy = colint [z,y]

-- Problem 36
        
intcol n = if n < 10 then [n] else (intcol $ quot n 10) ++ [(rem n 10)]
tobin n = if n < 2 then [n] else (tobin $ quot n 2) ++ [(rem n 2)]
palin n = (decl == reverse decl) && (binl == reverse binl)
  where decl = intcol n
        binl = tobin n
sol36 lim = sum $ filter palin [1..lim]









