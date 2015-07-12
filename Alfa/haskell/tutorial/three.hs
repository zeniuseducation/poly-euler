module Three where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

first :: [a] -> Maybe a
first [] = Nothing
first (x : _) = Just x

rest :: [a] -> [a]
rest [] = []
rest (x : xs) = xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x : xs) = x : take' (pred n) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (pred n) xs

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

rev' :: [a] -> [a]
rev' lst = iter lst []
	where
		iter [] res = res
		iter (x : xs) res = iter xs (x:res)

trev :: [a] -> [a]
trev [] = []
trev (x:[]) = [x]
trev xs = trev lpart ++ trev fpart
	where
		lcount = div (length xs) 2
		fpart = take lcount xs
		lpart = drop lcount xs


remove :: Eq a => a -> [a] -> [a]
remove elmt lst = iter lst []
	where
		iter [] res = res
		iter (x : xs) res
			| x == elmt = (rev' res) ++ xs
			| otherwise = iter xs (x:res)

removeAll :: Eq a => a -> [a] -> [a]
removeAll elmt lst = iter lst []
	where
		iter [] res = res
		iter (x : xs) res
			| x == elmt = (rev' res) ++ (removeAll elmt xs)
			| otherwise = iter xs (x:res)

member :: Eq a => a -> [a] -> Bool
member elmt lst = iter lst
	where
		iter [] = False
		iter (x:xs) = if x == elmt then True else False


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:[]) = [x]
qsort (x:xs) = qsort (filter (<= x) xs) ++ [x] ++ qsort (filter (> x) xs)

is_prime :: Integral a => a -> Bool
is_prime 2 = True
is_prime n = if odd n then iter 3 else False
	where
		lim = ceiling $ sqrt $ fromIntegral n
		iter i
			| i > lim = True
			| 0 == rem n i = False
			| otherwise = iter $ i+2

odd_prime :: Int -> Bool
odd_prime n = all (\x -> rem n x /= 0) $ takeWhile (\x-> x <= div n x) [3,5..]

sum_primes :: Int -> Int
sum_primes lim = sum $ filter odd_prime [3,5..lim]
