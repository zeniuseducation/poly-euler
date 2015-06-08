module Three where

import Data.List
import Data.Time
import qualified Data.Map as Map

toWords :: Int -> String
toWords n
  | n == 0 = ""
  | n < 10 = units !! (n-1)
  | 10 <= n && n <= 19 = teens !! (n-10)
  | 20 <= n && n <= 99 = (tens !! ((div n 10) - 2)) ++ toWords (rem n 10)
  | 100 <= n && n <= 999 = (units !! ((div n 100)-1))
                           ++ "hundred"
                           ++ if tunggal == 0 then "" else "and" ++ toWords tunggal
  | otherwise = "onethousand"
  where tunggal = rem n 100
        units = ["one", "two", "three", "four",
                 "five","six","seven","eight","nine"]
        teens = ["ten", "eleven", "twelve", "thirteen", "fourteen",
                 "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
        tens = ["twenty", "thirty", "forty", "fifty",
                "sixty", "seventy", "eighty", "ninety"]

sol17 :: Int -> Int
sol17 lim = sum $ map length $ map toWords [1..lim]

day (x,_,_) = x
month (_,x,_) = x
year (_,_,x) = x

nextSunday :: (Int,Int,Int) -> (Int,Int,Int)
nextSunday (d,m,y)
  | elem m [1,3,5,7,8,10,12] = tmonth (d+7, m, y) 31
  | elem m [4,6,9,11] = tmonth (d+7,m,y) 30
  | rem y 4 == 0 = tmonth (d+7,m,y) 29
  | otherwise = tmonth (d+7,m,y) 28

tmonth :: (Int,Int,Int) -> Int -> (Int,Int,Int)
tmonth (d,m,y) end
  | d <= end = (d,m,y)
  | m == 12 = (d-end, 1, y+1)
  | otherwise = (d-end,m+1,y)

euler19 :: (Int,Int,Int) -> Int
euler19 (d,m,y) = iter (d,m,y) 0
  where iter (dd,mm,yy) res
          | yy == 2001 = res
          | dd == 1 = iter (nextSunday (dd,mm,yy)) $ succ res
          | otherwise = iter (nextSunday (dd,mm,yy)) res

euler19b :: (Int,Int,Int) -> Int
euler19b dates = length $ filter (\ (a,b,c) -> a == 1) $
                 takeWhile (\ (a,b,c) -> c < 2001) $
                 iterate nextSunday dates

time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start


