module Two where

import Data.List

primes :: [Int]
primes = 2 : filter prime [3,5..]

prime :: Int -> Bool
prime p = all (\x-> 0 /= rem p x) $ takeWhile (\i-> i*i <= p) primes

prime100 :: [Int]
prime100 = take 25 primes

cham :: Int -> Int
cham lim = looper 0 25 prime100
  where looper prev cur res
          | prev == cur = cur + 1
          | otherwise = looper cur count lst 
          where lst = union res
                      [mul | x <- res , y <- prime100, let mul = x*y, mul <= lim]
                count = length lst

fcham n p lim
  | n*p > lim = 0
  | otherwise = succ $ sum $ map (\x -> fcham (n*p) x lim) bahan
  where bahan = dropWhile (< p) prime100

rcham lim = fcham 1 1 lim

idem :: Int -> Int
idem n = looper $ pred n
  where looper i = if i == rem (i * i) n then i else looper $ pred i

sol :: Int -> Int
sol lim = sum $ map idem [1..lim]

sol401 :: Integer -> Integer -> Integer
sol401 lim modi = foldl (\x y-> rem (x+y) modi) 0 $
                  map ssum $
                  takeWhile (\x -> x*x <= lim) [1..]
  where ssqr n = rem (div (n * (1 + n) * (1+ 2*n)) 6) modi
        ssum x = let limsum = div lim x
                 in rem ((ssqr limsum - ssqr x) +
                         ((x * x) * (limsum - (pred x)))) modi




