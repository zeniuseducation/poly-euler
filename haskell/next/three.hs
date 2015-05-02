module Three where 

import Data.List
import Data.Time
import Data.List.Split


sum_primes :: Int -> Int
sum_primes lim =
  let loop i res
        | i > lim = res
        | prime' i = loop (i+2) (res+i)
        | otherwise = loop (i+2) res
  in loop 3 2

euler1 :: Int -> Int
euler1 bts = sum [3,6..bts] + sum [5,10..bts] - sum [15,30..bts]

fibo 1 = 1
fibo 2 = 2
fibo n = (fibo (n-1)) + (fibo (n-2))

lfib = 1:2: (map (\ (x,y) -> x + y) $ zip lfib $ drop 1 lfib)

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
prime' n =
  let lim = ceiling $ sqrt $ fromIntegral n
      loopi i
        | i > lim = True
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

fromDigits xs = read (concat $ map show xs) :: Integer

identity mtx = mtx



read18 filename = do
  input <- readFile filename
  pyramid <-  map (\x -> read x :: [Int]) $
              map (\x -> "[" ++ (replace x ' ' ',') ++ "]") $ lines input
  result <- identity pyramid

replace st x r = map (\k -> if k == x then r else k) st

time f x = do
  start <- getCurrentTime
  f x
  stop <- getCurrentTime
  print $ diffUTCTime stop start









