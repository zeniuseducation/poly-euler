module Three where

import Data.List
import Data.Time
import qualified Data.Map as Map
import Common

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

bincol :: Int -> [Int]
bincol n = iter n []
  where iter i res
          | i < 2 = i : res
          | otherwise = iter (div i 2) (rem i 2 : res)

isPalin :: Eq a => [a] -> Bool
isPalin xs = xs == reverse xs

binPalin :: Int -> Bool
binPalin n = (isPalin . bincol) $ n

euler36 :: Int -> Int
euler36 lim = (listPalin oddConvert lim) + (listPalin evenConvert lim)
  where oddConvert n = let ncol = toDigits n
                       in toNumber $ ncol ++ (reverse $ init ncol)
        evenConvert n = let ncol = toDigits n
                        in toNumber $ ncol ++ (reverse ncol)
        listPalin f lim = sum $ filter binPalin $ map f [1..lim]

createFun a b = \x -> x^2 + a*x + b

consPrime :: (Int,Int) -> Int
consPrime (a,b) = let f = createFun a b
                  in length $ takeWhile prime' $ map f [0..]

euler27 :: Int -> (Int,Int)
euler27 lim = maxBy consPrime [(a,b)| a <- [-999,-997..lim], b <- primesUnder lim]

-- it returns true if xs is either a left/right-truncatable primes
-- left/right truncatability depends on function f, for left it's init
-- and tail for right
lrPrime xs f = all prime' $ map toNumber $ takeWhile (not.null) $ iterate f xs

lrPrime' n f = all prime' $ map toNumber $ takeWhile (not.null) $ iterate f $ toDigits n

euler37' :: Int -> Int
euler37' target = iter [2,3,5,7] []
  where iter bahan res
          | length res >= target = sum res
          | otherwise = iter lPrimes $ res ++ filter (\x -> lrPrime' x tail) lPrimes
          where lPrimes = filter (\x -> lrPrime' x init) [a*10+b | a <- bahan, b <- [1,3,7,9]]

euler37 :: Int -> Int
euler37 target = iter (map (\x -> [x]) [2,3,5,7]) []
  where iter bahan res
          | length res >= target = sum $ map toNumber res
          | otherwise = iter lPrimes $ res ++ filter (\x -> lrPrime x tail) lPrimes
          where lPrimes = filter (\x -> lrPrime x init)
                          [a++ [b] | a <- bahan, b <- [1,3,7,9]]

euler38 :: [Int] -> Int
euler38 xs = maximum $ map toNumber $ filter pandig' result
  where result = [[9]++a++ (toDigits $ 2*c) | a <- permutate xs 3, let c = toNumber $ 9:a]

time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start







