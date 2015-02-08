module Two where

import Data.List
import Data.Ord

fak :: Int -> Int -> Int
fak i j = foldl1 (*) [i..j]

is_prime :: Int -> Bool
is_prime p
  | p == 2 = True
  | even p = False
  | otherwise = looper 3
  where looper i
          | i*i > p = True
          | 0 == rem p i = False
          | otherwise = looper $ i+2

prime :: Int -> Bool
prime p
  | even p = False
  | otherwise = looper 3
  where looper i
          | i*i > p = True
          | 0 == rem p i = False
          | otherwise = looper $ i + 2

next_prime :: Int -> Int
next_prime p
  | p < 2 = 2
  | p == 2 = 3
  | even p = next_prime $ succ p
  | is_prime $ p + 2 = p + 2
  | otherwise = next_prime $ p + 2

primes :: [Int]
primes = iterate next_prime 2

sum_pfactors :: Int -> Int
sum_pfactors p = looper 0 p 0
  where looper :: Int -> Int -> Int -> Int
        looper m n res
          | (i * i) > n = res + n
          | 0 == rem n i = looper 0 (div n i) (res + i)
          | otherwise = looper (succ m) n res
          where i = (primes !! m)

comb :: Int -> Int -> Int
comb n k = div (fak (succ k) n) (fak 1 (n - k))

euler231 :: Int
euler231 = larger - smaller
  where larger :: Int
        larger = foldl1 (+) $ map sum_pfactors [15000001..20000000]
        smaller :: Int
        smaller = foldl1 (+) $ map sum_pfactors [2..5000000]

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x == y = x : (merge xs ys)
  | x <= y = x : (merge xs (y:ys))
  | otherwise = y : (merge (x:xs) ys)

hamming :: [Int] -> Int -> [Int]
hamming xs lim = looper xs xs []
  where looper l1 [] _ = l1
        looper l1 (l: []) pl = if pred then l1 else looper woka xs l1
          where waku = takeWhile (<= lim) $ map (* l) l1
                woka = merge l1 waku
                pred = length l1 == length pl
        looper l1 (l: ls) pl = looper (merge l1 waku) ls pl
          where waku = takeWhile (<= lim) $ map (* l) l1

euler204 :: Int -> Int
euler204 lim = length $ hamming (takeWhile (<= 100) primes) lim

combinations :: (Ord a, Eq a, Num a) => [a] -> Int -> [[a]]
combinations [] _ = []
combinations xs n
  | n == 1 = map (\x -> [x]) xs
  | otherwise = [sort $ x:y | x <- xs,
                 y <- combinations (delete x xs) (pred n)]

kpermute :: Integral a => [a] -> Int -> [[a]]
kpermute [] _ = []
kpermute xs n
  | n == 1 = map (\x -> [x]) xs
  | otherwise = [x:y | x <- xs, y <- kpermute (delete x xs) (pred n)]

selipin :: a -> Int -> [a] -> [a]
selipin elm idx xs = (take (pred idx) xs)
                     ++ [elm] ++ (drop (pred idx) xs)

colnum :: [Int] -> Int
colnum [] = 0
colnum (x: []) = x
colnum xs = looper xs 0
  where looper (x: []) res = (10 * res) + x
        looper (x: xs) res = looper xs $ (10 * res) + x

genPrimes :: Int -> Int -> [Int]
genPrimes dig n = filter (>= 10^n) res
  where bast = delete dig [0..9]
        l = take n $ repeat dig
        res = [colnum $ selipin x i l | x <- bast , i <- [1.. succ n]]

genNumbers :: Int -> Int -> Int -> [Int]
genNumbers dig ndig tdig
  | tdig <= ndig = take tdig $ repeat dig
  | tdig - ndig == 1 = filter (>= 10^ndig) res
  | otherwise = looper bres $ succ ndig
  where bast = delete dig [0..9]
        bres = [selipin x i l |
                x <- bast, i <- [1..succ ndig]]
        l = take ndig $ repeat dig
        res = map colnum bres
        looper xs n
          | n >= tdig =  filter (>= 10^ (pred tdig)) $ map colnum xs
          | otherwise = looper lres $ succ n
          where lres = [selipin x i ls |
                        x <- bast, i <- [1..succ $ length $ head xs],
                        ls <- xs]

findPrimes :: Int -> Int -> Int -> Int
findPrimes dig tdig ndig
  | null tmp = findPrimes dig tdig $ pred ndig
  | otherwise = sum tmp
  where res = nub $ genNumbers dig ndig tdig
        tmp = filter prime res

sol111 :: Int -> Int
sol111 t = sum $ map (\x -> findPrimes x t (pred t)) [0..9]
                

numcol :: Int -> [Int]
numcol n = if n < 10 then [n] else (numcol $ div n 10) ++ [rem n 10]

genResults :: Int -> Int -> [Int]
genResults dig tdig
  | null temp = looper bas
  | otherwise = temp
  where temp = filter prime bas
        bas = genPrimes dig tdig
        looper xs
          | null tmp = looper bas1
          | otherwise = tmp
          where brak = map numcol xs
                bas1 = [colnum $ selipin x i l |
                        x <- (delete dig [0..9]),
                        i <- [1..succ $ length $ head brak],
                        l <- brak]
                tmp = filter prime bas1
                
sol145 :: Int -> Int
sol145 lim = length $ filter (\x -> all odd $ bar x) [1..pred lim]
  where bar i = if 0 /= head basa 
                then numcol $ sum [i, colnum basa]
                else [2]
          where basa = reverse $ numcol i

sol145b :: Int -> Int
sol145b lim = looper 1 0
  where looper :: Int -> Int -> Int
        looper i res
          | i >= lim = res
          | valid i = looper (succ i) (succ res)
          | otherwise = looper (succ i) res
          where valid i
                  | rem i 10 == 0 = False
                  | all odd pri = True
                  | otherwise = False
                  where pri = numcol $ sum [i, colnum $ reverse $ numcol i]





