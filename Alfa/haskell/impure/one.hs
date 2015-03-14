module One where 

import Data.List
import Data.Time

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

time f x = do
  start <- getCurrentTime
  let res = f x
  print res
  stop <- getCurrentTime
  print $ diffUTCTime stop start


