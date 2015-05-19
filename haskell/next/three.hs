module Three where 

import Data.List
import Data.Time
import Data.List.Split
import qualified Data.Set as Set


sum_primes :: Int -> Int
sum_primes lim =
  let loop i res
        | i > lim = res
        | prime' i = loop (i+2) (res+i)
        | otherwise = loop (i+2) res
  in loop 3 2

euler1 :: Int -> Int
euler1 bts = sum [3,6..bts] + sum [5,10..bts] - sum [15,30..bts]

lfib = 1:2: zipWith (+) lfib (tail lfib)

euler2 :: Int -> Int
euler2 bates = sum $ filter even $ takeWhile (< bates) lfib

-- returns the largest prime factor of n
euler3 :: Int -> Int
euler3 n = loop n 3
  where loop p i
          | p == 1 = i-2
          | prime' i = if 0 == rem p i
                       then loop (div p i) (i+2)
                       else loop p (i+2)
          | otherwise = loop p (i+2)

isPalindrome :: Int -> Bool
isPalindrome n = let strn = show n
                 in strn == reverse strn

euler4 :: Int -> Int
euler4 bates = maximum $
               filter isPalindrome $
               [x*y| x <- [900..bates], y <- [900..bates]]

euler5 :: Int -> Int
euler5 bates = foldl lcm 1 [2..bates]

square :: Integral a => a -> a
square n = n * n

euler6 :: Int -> Int
euler6 bts = (sum $ map square [1..bts]) - ((square.sum) $ [1..bts])


euler7 :: Int -> Int
euler7 = (!!) (2:filter prime' [3,5..])

euler9 bates = [a*b*c | a <- [3.. (div bates 4)], b <- [a+1.. (div bates 2)],
                let c = bates-a-b, c^2 == a^2 + b^2]

prime' :: Int -> Bool
prime' n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = let loopi i
                      | i*i > n = True
                      | 0 == rem n i = False
                      | otherwise = loopi $ i + 2
                in loopi 3
     
euler10 :: Int -> Int
euler10 bates = sum $ filter prime' $ 2: [3,5..bates]

euler11 :: [[Int]] -> Int
euler11 xs = maximum [left,right, ldiag, rdiag] 
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
      res = euler11 tmp
  print res


countDivisors :: Int -> Int
countDivisors 1 = 1
countDivisors n = let bates = ceiling $ sqrt $ fromIntegral n
                      smalldiv = filter (\x -> 0 == rem n x) [1..bates]
                      lsmall = last smalldiv
                  in if square lsmall == n
                     then 2* (length smalldiv) - 1
                     else 2* (length smalldiv)

cdivTriangle :: Int -> Int
cdivTriangle n = if even n
                 then (countDivisors (div n 2)) * (countDivisors (n+1))
                 else (countDivisors (div (n+1) 2)) * (countDivisors n)

euler12 :: Int -> Int
euler12 target = let (x:xs) = dropWhile (\x -> (cdivTriangle x) < target) [12..]
                 in if even x then div (2*x* (2*x+1)) 2 else div (x* (x+1)) 2

max13 str = maximum $
            map (\x -> product $ map (\k -> read [k] :: Int) x) $
            map (\x -> take 13 x) $
            takeWhile (\x -> (length x) >= 13) $
            iterate tail str 

euler8 filename = do
  input <- readFile filename
  print $ max13 $ concat $ lines input

euler13 :: [String] -> String
euler13 xs = take 10 $ show $ sum $ map (\x -> read x :: Integer) xs

read13 filename = do
  input <- readFile filename
  print $ euler13 $ lines input

collatz :: Int -> Int
collatz 1 = 1
collatz n = if even n
            then 1 + (collatz $ div n 2)
            else 1 + (collatz $ 3 * n + 1)

maxBy f (x: []) = x
maxBy f (x: xs) = let nxt = maxBy f xs
                  in if f x > f nxt then x else nxt

euler14 :: Int -> Int
euler14 bates = maxBy collatz [500001,500003..bates]

pascal = (!!) $ iterate (\x -> map (\ (k,l) -> k + l) $ zip (0:x) (x ++ [0])) [1]

euler15 :: Int -> Int
euler15 n = sum $ map square $ pascal n

euler16 n = sum $ map (\x -> read [x] :: Int) $ show n

toDigits n = map (\x -> read [x] :: Int) $ show n

fromDigits xs = read (concat $ map show xs) :: Int

replace st x r = map (\k -> if k == x then r else k) st

euler18 (x: []) = head x
euler18 (x:y:xs) = euler18 $ (map (\ (k,l) -> k+l) mx) : xs
  where mx = zip y $ map (\ (k,l) -> max k l) $ zip x $ tail x

read18 filename = do
  input <- readFile filename
  let pyramid = reverse $ map (\x -> read x :: [Int]) $
                map (\x -> "[" ++ (replace x ' ' ',') ++ "]") $ lines input
  print $ euler18 pyramid

euler20 n = sum $ map (\x-> read [x] :: Int) $ show $ product [1..n]

isPerfect n = let m = sqrt $ fromIntegral n
              in ceiling m == floor n

sumProper :: Int -> Int
sumProper n = if even n then loop 2 1 1 else loop 3 1 2
  where loop a acc d
          | a*a > n = acc
          | a*a == n = acc+a
          | 0 == rem n a = loop (a+d) (acc+a+ (div n a)) d
          | otherwise = loop (a+d) acc d

isAmicable :: Int -> Bool
isAmicable n = let m = sumProper n
               in sumProper m == n && m /= n

euler21 :: Int -> Int
euler21 upper = sum $ filter isAmicable [2..upper]

read22 filename = do
  input <- readFile filename
  let m = takeWhile (\x-> '\n' /= x) $ input
  print $ m

isAbundant :: Int -> Bool
isAbundant n = n < sumProper n

abuns :: [Int]
abuns = filter isAbundant [12..]

euler23 :: Int -> Int
euler23 bts = (sum [1..bts]) - (loop bahan Set.empty)
  where bahan = takeWhile (<= bts) abuns
        loop [] abunSet = sum $ Set.toList $ abunSet
        loop (x:xs) abunSet = loop xs $ Set.union abunSet $ loopi (dropWhile (< x) bahan) []
          where loopi (k:ks) result
                  | x+k > bts = Set.fromList result
                  | otherwise = loopi ks ((x+k) : result) 

euler24 n = fromDigits $ (sort $ permutations [0..9]) !! n

euler25 target = fibo 1 0 1 
  where fibo a b i = if a > target then i else fibo (a+b) a (i+1)

recDecimal n = loop texp [] []
  where texp = 10^ (ceiling $ logBase (fromIntegral 10) (fromIntegral n))
        loop divisee result remainders
          | elem remainder remainders = length $ result
          | otherwise = loop (texp * remainder) (res:result) (remainder:remainders)
          where res = div divisee n
                remainder = divisee - (res * n)

euler26 bound = loop bound 0 0
  where loop i current result
          | result > i = [current,result]
          | curResult > result = loop (i-2) i curResult
          | otherwise = loop (i-2) current result
          where curResult = recDecimal i

primesUnder :: Int -> [Int]
primesUnder n = takeWhile (< n) $ filter prime' [3,5..]

countPrimes :: Int -> Int -> Int
countPrimes a b = length $ takeWhile prime' $
                  map (\x -> (square x) + (a * x) +b) [0..]

sumCorner :: Int -> Int
sumCorner n = sum $ take 4 $ iterate (\x -> x - (n-1)) $ square n

euler28 :: Int -> Int
euler28 target = succ $ sum $ map sumCorner [3,5..target]

euler29 :: Integral a => a -> Int
euler29 target = length $ nub [a^b | a <- [2..target], b <- [2..target]]

sumdigFive :: Int -> Int
sumdigFive n = sum $ map (^5) $ toDigits n

euler30 :: Int -> Int
euler30 upper = sum $ filter (\x -> x == sumdigFive x) [10..upper]

coins = [1,2,5,10,20,50,100,200]

sumCoins :: Int -> Int -> Int
sumCoins amount coin
  | amount == 0 = 1
  | coin == 0 = 1
  | otherwise = sum $ map (\x -> sumCoins (amount - (vcoin * x)) (pred coin)) ncoins
  where vcoin = coins !! coin
        ncoins = takeWhile (\k -> (vcoin * k) <= amount) [0..]

euler31 :: Int -> Int
euler31 tar = sumCoins tar 7

pandigProd :: Int -> Int -> Int
pandigProd a b = if pandig then prod else 0
  where prod = a*b
        pandig = [1..9] == (sort $ concat [toDigits a, toDigits b, toDigits prod])

euler32 :: Int -> Int
euler32 tar = sum $ nub $ loop 1 Set.empty
  where loop a result
          | a > tar = Set.toList result
          | otherwise = loop (succ a) $ Set.union result $ loopi (succ a) []
          where loopi b resb
                  | a*b > tar = Set.fromList resb
                  | pandig == 0 = loopi (succ b) resb
                  | otherwise = loopi (succ b) ((a*b) : resb)
                  where pandig = pandigProd a b

factorials = map (\x -> product [1..x]) [0..]

factorial :: Int -> Int
factorial = (!!) factorials

digFactorial :: Int -> Int
digFactorial n = if facto then n else 0
  where facto = n == (sum $ map factorial $ toDigits n)

euler34 :: Int -> Int
euler34 bates = sum $ filter (\x -> 0 /= x) $ map digFactorial [10..bates]

isCprime :: [Int] -> Bool
isCprime xs = loop xs []
  where loop [] res = prime' $ fromDigits res
        loop (k:ks) res
          | prime' $ fromDigits $ ks ++ res ++ [k] = loop ks (res ++ [k])
          | otherwise = False
                        
euler35 :: Int -> Int
euler35 bates = (loop [[1],[7],[3],[9]] 1 [[1],[7],[3],[9]])
  where dbates = ceiling $ logBase 10 $ fromIntegral bates
        loop xss lim res 
          | lim > dbates = length res
          | otherwise = loop mentah (succ lim) (res ++ hasil)
          where mentah = [xs ++ [x] | xs <- xss, x <- [1,3,7,9]]
                hasil = filter isCprime mentah

numToBin :: Int -> [Int]
numToBin n
  | n < 2 = [n]
  | otherwise = (rem n 2) : numToBin (div n 2)

isPalin :: Eq a => [a] -> Bool
isPalin xs = xs == reverse xs

euler36 :: Int -> Int
euler36 lim = sum $ filter (\x-> (isPalin $ numToBin x) && isPalindrome x) [1..lim]

time f x = do
  start <- getCurrentTime
  print $ f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start









