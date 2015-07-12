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
