import Data.List
import Data.Time

primes :: [Int]
primes = 2: (filter (\x -> (all (\y -> 0 /= rem x y) $ takeWhile (\k -> k*k <= x) primes)) [3..])

prime' :: Int -> Bool
prime' x = all (\k -> 0 /= rem x k) $ takeWhile (\i -> i*i <= x) primes

fibos :: [Integer]
fibos = 1:1:map lfibo [2..]

fibo :: (Int -> Integer)
fibo = (!!) fibos

lfibo :: Int -> Integer
lfibo i = (fibo (pred i)) + (fibo (i - 2))

limFibo :: Integer -> Integer -> Int -> Integer -> Int
limFibo a b i lim = if a > lim then i else limFibo (a+b) a (succ i) lim

repeating :: Int -> Int
repeating lim = loopi lim (1000,0)
  where loopi i ns@(num,res)
          | i < res = num
          | even i = loopi (pred i) ns
          | prime' i = let tmp = check i 0 1000 []
                       in if tmp > res
                          then loopi (i-2) (i,tmp)
                          else loopi (i-2) ns
          | otherwise = loopi (head $ dropWhile (not.prime') [i-2,i-4..]) ns
          where check x resi diva xs =
                  let t1 = div diva x
                      t2 = rem diva x
                  in if elem t2 xs
                     then resi
                     else check x (succ resi) (10*t2) (t2:xs)

evenFibo :: Int -> Int
evenFibo = loopi 1 0 0
  where loopi a b res lim
          | a > lim = res
          | even a = loopi (a+b) a (res+a) lim
          | otherwise = loopi (a+b) a res lim

maxPFactor :: Int -> Int
maxPFactor = loopi
  where loopi p =
          let tmp = head $ dropWhile (\i -> 0 /= rem p i) primes
          in if tmp == p then tmp else loopi (div p tmp)

digSquare :: Int -> Int
digSquare n = loopi n 0
  where loopi i res = if i < 10
                      then res + i*i
                      else loopi (div i 10) (res + (im*im))
          where im = rem i 10

digSqr :: Int -> Int
digSqr = digSquare

maxi (x: []) = x
maxi (x:xs) = if x > maxim then x else maxim
  where maxim = maxi xs

qsort [] = []
qsort (x: []) = [x]
qsort (a:b: []) = (min a b): (max a b) : []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [i | i <- xs, i <= x]
        larger = [i| i <- xs, i > x]

mul35 = filter (\x -> rem x 3 == 0 or rem x 5) [1..]

sol1 lim = sum $ takeWhile (<lim) mul35

main = do
   start <- getCurrentTime
   let result = maxPFactor 600851475143
       in putStrLn $ show result
   stop <- getCurrentTime
   print $ diffUTCTime stop start



