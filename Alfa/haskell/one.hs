import Data.List
import Data.Time

import qualified Data.Set as Set



oddPrime :: Int -> Bool
oddPrime n = loop 3
  where loop :: Int -> Bool
        loop i
          | i*i > n = True
          | rem n i == 0 = False
          | otherwise = loop $ i+2

nthPrime :: Int -> Int
nthPrime n = loopi 3 2
  where loopi res i
          | i == n = res
          | otherwise = loopi xres $ succ i
          where (xres:xs) = dropWhile (not . oddPrime) [res+2,res+4..]

sumPrimes :: Int -> Int
sumPrimes lim = loopi 3 2
  where loopi :: Int -> Int -> Int
        loopi i res
          | i > lim = res
          | oddPrime i = loopi (i+2) (i+res)
          | otherwise = loopi (i+2) res

testo = do
  res <- readFile "one.txt"
  start <- getCurrentTime
  let tres = sum $ map (\x -> read x :: Integer) $ words res
      kres = show tres
      pell = length kres
      rres = div tres $ 10^ (pell-10)
  stop <- getCurrentTime
  putStrLn $ show rres
  print $ diffUTCTime stop start

findMax :: [Int] -> Int
findMax xs = maximum $ take 987 $  map (\x-> product $ take 13 x) $ iterate tail xs

prob8 = do
  res <- readFile "p8.txt"
  let kres = findMax $
             map (\x-> read x :: Int) $
             concatMap (map (\y-> [y])) $
             words res
  print kres

sumproper :: Int -> Int
sumproper n = looper 2 1
  where looper :: Int -> Int -> Int
        looper i res
          | i*i >= n = if i*i == n then res + i else res
          | rem n i == 0 = looper (1+i) (res+i + (div n i))
          | otherwise = looper (1+i) res

isAmic :: Int -> Bool
isAmic n =
  let sp = sumproper n
  in n == sumproper sp && n /= sp

qsort (x:xs) = smaller ++ [x] ++ larger
  where smaller = [y | y <- xs, y <= x]
        larger = [y | y <- xs, y > x]

lprime :: Int -> Int
lprime n = let looper :: Int -> Int -> Int
               looper p i
                 | even p = looper (div p 2) i
                 | oddPrime p = p
                 | 0 == rem p i = looper (div p i) 3
                 | otherwise = looper p (head $ dropWhile (not.oddPrime) [i+2,i+4..])
           in looper n 3

colnum2 :: [Int] -> Int
colnum2 xs = looper xs 0
  where looper [] _ = 0
        looper (x: []) res = x + (2 * res)
        looper (x:xs) res = looper xs $ 2*res + x

permutes :: [a] -> Int -> [[a]]
permutes xs k = looper 1 $ [[x]| x <- xs]
  where looper i res =
          let nres = [r ++ [l] | r <- res, l <- xs]
          in if i == k
             then res
             else looper (i+1) nres

main = do
  start <- getCurrentTime
  print $ lprime 600851475143
  stop <- getCurrentTime
  print $ diffUTCTime stop start

  


