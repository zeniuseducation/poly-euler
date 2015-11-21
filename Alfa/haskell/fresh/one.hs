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

sum_digits :: Integral a => a -> a
sum_digits x = if x < 10
               then x
               else (rem x 10) + sum_digits (div x 10)

primes :: [Int]
primes = 2 : sieves [3,5..]

sieves :: [Int] -> [Int]
sieves (x:xs) = x : filter (\k -> 0 /= rem k x) (sieves xs)

sol15 :: Int -> Int
sol15 size = sum $
             map (\x-> x*x) $
             head $
             drop size $
             iterate (\x -> map (\(a,b) -> a+b) $ zip (0:x) (x ++ [0])) [1]

factorial :: Integral a => a -> a
factorial n = product [1..n]

sol46 :: Int -> Int
sol46 lim = loopi 9
  where loopi :: Int -> Int
        loopi i = if prime' i
                  then loopi $ i + 2
                  else if loopj 1 then loopi $ i + 2 else i
          where loopj :: Int -> Bool
                loopj j = let tmpj = 2 * j * j
                          in if tmpj >= i
                             then False
                             else if prime' $ i - tmpj
                                  then True
                                  else loopj $ succ j

modex :: Integral a => (a -> a -> a -> a)
modex a b modi
  | b == 0 = 1
  | b == 1 = a
  | even b = rem (modexi*modexi) modi
  | otherwise = rem (a*modexi*modexi) modi
  where modexi = modex a (div b 2) modi

sol48 :: Integral a => a -> a -> a
sol48 lim modi = rem (sum $ map (\x-> modex x x modi) [1..lim]) modi


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

gesek n ms mk f
  | f <= n * ms = (- f, "statis")
  | otherwise = (- (n * mk), "kinetis")

time f = do
  start <- getCurrentTime
  print $ f
  stop <- getCurrentTime
  print $ diffUTCTime stop start
