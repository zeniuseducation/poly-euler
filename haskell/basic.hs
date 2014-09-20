module Basic where

import Data.List
import Data.Ord

expt' a m
  | m == 0 = 1
  | otherwise = a * (expt' a (pred m))

sum' (x:xs)
  | null xs = x
  | otherwise = x + sum' xs

prod' (x:xs)
  | null xs = x
  | otherwise = x * prod' xs

fibo' i
  | i <= 2 = 1
  | otherwise = fibo' (pred i) + fibo' (i-2)

fibolist 1 = [1]
fibolist 2 = [1,1]
fibolist i = (l1 + l2) : (l1:l2:res)
  where (l1:l2:res) = fibolist (pred i)

fibo i = head $ fibolist i

fibolistHelper i j (l1:l2:res)
  | i == j = (l1:l2:res)
  | otherwise = fibolistHelper i (succ j) ((l1+l2):(l1:l2:res))

fibolist' i
  | i == 1 = [1]
  | otherwise = fibolistHelper i 2 [1,1]

primeTolol' p = null [x | x <- [2..(pred p)], 0 == rem p x]

prime' p =  null [x | x <- [2..(succ (div p 2))], 0 == rem p x]

prime'' 2 = True
prime'' p = null [x | x <- [2..proot], 0 == rem p x]
  where proot = (ceiling.sqrt) (fromIntegral p)

prime1 2 = True
prime1 p
  | even p = False
  | otherwise = null [x | x <- [3,5..proot], 0 == rem p x]
  where proot = (ceiling.sqrt) (fromIntegral p)

fibolist2 i = last $ take (pred i)
              $ iterate (\(x:y:xs) -> (x+y):(x:y:xs)) [1,1]

sumfibo i = last $ last $ take (pred i)
            $ iterate (\(x:y:xs:l) -> [(x+y),x,(x+y+xs)]) [1,1,2]

iterateHelper f x = (f x): (iterateHelper f (f x))
  
iterate' f x = x : iterateHelper f x










