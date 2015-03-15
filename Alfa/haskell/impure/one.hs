module One where 

import Data.List
import Data.Time
import Data.List.Split

prime' :: Int -> Bool
prime' n =
  let lim = ceiling $ sqrt $ fromIntegral n
      loopi i
        | i > lim = True
        | 0 == rem n i = False
        | otherwise = loopi $ i + 2
  in loopi 3

sum_primes :: Int -> Int
sum_primes lim =
  let loop i res
        | i > lim = res
        | prime' i = loop (i+2) (res+i)
        | otherwise = loop (i+2) res
  in loop 3 2

createTriad :: String -> [[Int]]
createTriad raw =
  reverse $ map (\x -> map (\k -> read k ::Int) x) $ map words $ lines raw

sol18 :: String -> Int
sol18 raw = loop (createTriad raw) []
  where loop (x1:x2:xs) res
          | null xs = (head x2) + (maximum res)
          | otherwise = loop (bega:xs) bega
          where bega :: [Int]
                bega = map (\(x,y) -> x+y) $
                       zip x2 $
                       map (maximum.take 2) $
                       takeWhile (\x -> (length x) >= 2) $
                       iterate tail x1

readp18 = do
  input <- readFile "p67.txt"
  let tmp = sol18 input
  return tmp

sol22 raw =
  let bahan :: [(String,Int)]
      bahan = zip raw [1..]
      refs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..]
      allInt x = case x of
                      Just x -> x
                      Nothing -> 0
      score (x,sc) = sc * (sum $ map allInt (map (\k -> lookup k refs) x))
  in sum $ map score bahan

readp22 = do
  input <- readFile "p22.txt"
  let tmp = sort $ read input :: [String]
      res = sol22 tmp
  return res

fibo :: Integer -> Int
fibo lim = loop 1 0 1
  where loop :: Integer -> Integer -> Int -> Int
        loop a b i = if a > lim then i else loop (a+b) a (1+i)

sol4 :: Int -> Int
sol4 upper =
  let iskali x =
        let ks@(k:_) = dropWhile (\i -> 0 /= rem x i) [upper, pred upper..]
        in if null ks
           then False
           else
             let tmp = div x k
             in tmp <= upper && k /= tmp
      cpalin i = read ((show i) ++ (reverse (show i))) :: Int
  in head $ dropWhile (not.iskali) $ map cpalin [upper,pred upper..1]

sol4b :: Int -> Int -> Int
sol4b lower upper = maximum sumber
  where sumber = [num| i <- [upper,upper-1..lower], j <- [upper,upper-1..lower],
                  let num = i*j, ispalin num]
        ispalin num = let numst = show num in numst == reverse numst

sol11 :: [[Int]] -> Int
sol11 xs = maximum [left,right, ldiag, rdiag] 
  where left = maximum $
               map (\k -> maximum $
                          map (product.take 4) $
                          takeWhile (\x -> length x >= 4) $
                          iterate tail k) xs
        right = maximum $
                map (\k -> maximum $
                           map (product.take 4) $
                           takeWhile (\x -> length x >= 4) $
                           iterate tail k) $
                transpose xs
        ldiag = maximum [product $ map (\(x,y) -> (xs!!x)!!y) (zip [i..i+3] [j..j+3]) |
                         i <- [0..16], j <- [0..16]]
        rdiag = maximum [product $ map (\(x,y) -> (xs!!x)!!y) (zip [i..i+3] [j,j-1..j-3]) |
                         i <- [0..16], j <- [3..19]]

readp11 = do
  input <- readFile "p11.txt"
  let tmp = map (\x -> map (\k -> read k :: Int) x) $
            map (take 20) $
            takeWhile (\x-> (length x) >= 20) $
            iterate (drop 20) $
            words input
      res = sol11 tmp
  print res

readp13 = do
  input <- readFile "p13.txt"
  let tmp = take 10 $ show $ sum $ map (\x -> read x :: Integer) $ words input
  print tmp
      

time f = do
  start <- getCurrentTime
  f
  stop <- getCurrentTime
  print $ diffUTCTime stop start


