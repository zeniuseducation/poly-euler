module Three where

import Data.List
import Data.Time
import qualified Data.Map as Map
import qualified Data.Set as Set
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


digNums = scanl1 (+) (0:zipWith (*) [1..] (map (\x -> 9*10^x) [0..]))

refs = map (\x -> 10^x - 1) [0..]

numDigs :: Integer -> [Integer]
numDigs i = iter i []
  where iter n res
          | n < 10 = n:res
          | otherwise = iter (div n 10) (rem n 10 : res)

digNum :: Int -> Int
digNum i = hasil
  where weter = takeWhile (< i) digNums
        numOfs = length weter
        jums = (-) i (last weter)
        rems = rem jums numOfs
        divs = if rems == 0
               then pred $ div jums numOfs
               else div jums numOfs
        hasil = (toDigits $ ((!!) refs (pred numOfs) + (succ divs))) !! if rems == 0 then pred numOfs else (pred rems)

-- this one is very very cool 0.05ms
sol40 :: [Int] -> Int
sol40 xs = product $ map digNum $ map (10^) xs

pandig :: Int -> Bool
pandig n = xs == [1..7]
  where xs = sort $ toDigits n

sol41 :: Int -> Int
sol41 start = maximum $ filter pandig $ filter prime' [start..10^7]

-- this one runs in only 13ms
sol41b :: [Int] -> Int
sol41b lim = maximum $ filter prime' $ map toNumber $ permutations lim

modex :: Int -> Int -> Int -> Int
modex a m modi
  | m == 0 = 1
  | m == 1 = rem a modi
  | even m = rem (tmp * tmp) modi
  | otherwise = rem (a * tmp * tmp) modi
  where tmp = modex a (div m 2) modi

sol48 modi = rem (sum [modex i i modi | i <- [1..1000]]) modi

rapihin :: String -> [String]
rapihin st = iter st [] []
  where iter [] resi res = filter (not.null) (resi:res)
        iter (x:xs) resi res
          | elem x ['A'..'Z'] = iter xs (x:resi) res
          | otherwise = iter xs [] (resi:res)

value :: String -> Int
value xs = sum $ map convert $ map (\x -> lookup x mp) xs
  where mp = zip ['A'..'Z'] [1..]
        convert x = case x of
          Just x -> x
          Nothing -> 0

triangles :: [Int]
triangles = scanl1 (+) [1..]

elemMax :: Int -> [Int] -> Bool
elemMax x xs = elem x $ takeWhile (<= x) xs

-- runs in 10ms
readp42 = do
  input <- readFile "p42.txt"
  let tmp = length $ filter (\x-> elemMax x triangles) $ map value $ rapihin input
  return tmp

sol43 :: [Int] -> Int
sol43 bahan = loopi (permutate bahan 3) 2 1
  where sbahan = Set.fromList bahan
        loopi bhn p r
          | p == 19 = sum $ map toNumber bhn
          | otherwise = loopi remo (nextPrime p) $ succ r
          where result = [xs++ [x] | xs <- bhn , x <- Set.toList (Set.difference sbahan (Set.fromList xs))]
                remo = filter (\x -> 0 == rem (toNumber (drop r x)) p) result
                
sol49 :: Int -> Int
sol49 diff = loopi (nextPrime 1000)
  where loopi n
          | n == 1487 = loopi $ n + 2
          | prime' n && permutation' n1 n && prime' n1 && permutation' n2 n && prime' n2 =
            toNumber $ (toDigits n) ++ (toDigits n1) ++ (toDigits n2)
          | otherwise = loopi $ n + 2
          where n1 = n + diff
                n2 = n1 + diff 

time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start







