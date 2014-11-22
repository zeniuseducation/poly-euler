module One where

import Data.List

numcol n
  | n < 10 = [n]
  | otherwise = (numcol $ div n 10) ++ [rem n 10]

colnum ls = helper ls 0
  where helper (x:xs) res
          | null xs = x + (10*res)
          | otherwise = helper xs (x+ 10*res)

triangles = map (\n -> div (n * (succ n)) 2) [1..]
squares = map (\x -> x*x) [1..]
pentas = map (\n -> div (n * (pred (3*n))) 2) [1..]
hexas = map (\n -> n * (pred $ 2*n)) [1..]
heptas = map (\n -> div (n * (5*n - 3)) 2) [1..]
octas = map (\n -> n * (3*n - 2)) [1..]

figurates ls = map (\x -> [div x 100, rem x 100]) $ result
  where result = takeWhile (< 10000) $ dropWhile (< 1000) ls

figures = map figurates [triangles, squares, pentas, hexas, heptas]

search_one ls = map (\ (x:y: []) -> helper (x:y: []) figures []) ls
  where helper (a:b:_) (fs:fss) res
          | null fss = res ++ bliter
          | null bliter = res
          | otherwise = helper (head bliter) fss (res ++ [head bliter])
          where bliter = filter (\ (m:n:_) -> a == n) fs

collatz :: Int -> Int
collatz n
  | n == 1 = 1
  | even n = 1 + collatz (div n 2)
  | otherwise = 1 + collatz (1 + (3 * n))

max_collatz :: Int -> Int -> Int
max_collatz start lim = helper start 1 1
  where helper :: Int -> Int -> Int -> Int
        helper i res lres
          | i > lim = res
          | colls > lres = helper (2 + i) i colls
          | otherwise = helper (2 + i) res lres
          where colls = collatz i


sum_pdivs :: Int -> Int
sum_pdivs n
  | even n = helper_even 2 1
  | otherwise = helper_odd 3 1
  where helper_even :: Int -> Int -> Int
        helper_even i res
          | (i*i) > n = res
          | 0 == (rem n i) = if div n i == i
                             then res + i
                             else helper_even (1+i) (i+res+ (div n i))
          | otherwise = helper_even (succ i) res
        helper_odd :: Int -> Int -> Int
        helper_odd i res
          | (i*i) > n = res
          | 0 == (rem n i) = if div n i == i
                             then res + i
                             else helper_odd (2 + i) (i + res + (div n i))
          | otherwise = helper_odd (succ i) res

sum_amic :: Int -> Int
sum_amic lim = helper 2 0
  where helper :: Int -> Int -> Int
        helper i res
          | i >= lim = res
          | i == amic = helper (succ i) res
          | i == (sum_pdivs amic) = helper (succ i) (i + res)
          | otherwise = helper (succ i) res
          where amic :: Int
                amic = sum_pdivs i
                
fibolim :: Integer -> Int
fibolim lim = helper 1 1 1
  where helper :: Integer -> Integer -> Int -> Int
        helper i j idx
          | i > lim = idx
          | otherwise = helper (i+j) i (succ idx)

sum_primes lim = 2 + (sum $ filter prime' [3,5..lim])
  where prime' n = all (\x -> 0 /= rem n x) (takeWhile (\x -> x*x <= n) [3,5..]) 


nth_prime :: Int -> Int
nth_prime i = (filter prime' [3,5..]) !! (i - 2)
  where prime' :: Int -> Bool
        prime' n = all (\x -> 0 /= rem n x) (takeWhile (\x -> x*x <= n) [3,5..])

euler28 lim = succ $ sum $ map (\x -> 2 * (x^2 + (x^2 - (3 * (x - 1))))) [3,5..lim]
euler28a lim = 1 + sum [2*(x^2 + (x^2 - (3* (x-1))))| x <- [3,5..1001]]

prime' :: Int -> Bool
prime' n = all (\x -> 0 /= rem n x) (takeWhile (\x -> x*x <= n) [3,5..])

next_prime :: Int -> Int
next_prime n
  | n == 2 = 3
  | prime' (n+2) = n+2
  | otherwise = next_prime (n+2)


prev_prime :: Int -> Int
prev_prime n
  | n == 3 = 2
  | prime' (n-2) = n-2
  | otherwise = prev_prime (n-2)

is_prime :: Int -> Bool
is_prime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (\x -> 0 /= rem n x) (takeWhile (\x -> x*x <= n) [3,5..])

primes_under :: Int -> [Int]
primes_under lim = takeWhile (< lim) $ iterate next_prime 2

euler27 :: Int -> Int
euler27 lim = result (blooper 997 []) [0,0,0]
  where result :: [[Int]] -> [Int] -> Int
        result [] (rr:ra:rb:_) = ra * rb
        result ((ir:ar:br:_):xs) (rr:ra:rb:_)
          | ir > rr = result xs [ir,ar,br]
          | otherwise = result xs [rr,ra,rb]
        blooper :: Int -> [[Int]] -> [[Int]]
        blooper b resb
          | b >= lim = resb
          | otherwise = blooper (next_prime b)
                        ((alooper (- lim) (- lim) 1) : resb)
          where alooper :: Int -> Int -> Int -> [Int]
                alooper a cura resa
                  | 1 + a + b <= 0 = alooper (succ a) cura resa
                  | a > lim = [resa, cura, b]
                  | resn > resa = alooper (succ a) a resn
                  | otherwise = alooper (succ a) cura resa
                  where resn :: Int
                        resn = nlooper 1 1
                          where nlooper :: Int -> Int -> Int
                                nlooper n res
                                  | is_prime $ (n^2) + (a*n) + b = nlooper (n+1) (1+res)
                                  | otherwise = res 

euler27b :: Int -> Int
euler27b lim = blooper 997 [0,0,0]
  where blooper :: Int -> [Int] -> Int
        blooper b (rr:ra:rb:_)
          | b < rr = ra*rb
          | iresa > rr = blooper (prev_prime b) [iresa,icura,ib]
          | otherwise = blooper (prev_prime b) [rr,ra,rb]
          where (iresa:icura:ib: []) = alooper (- lim) (- lim) 1
                  where alooper :: Int -> Int -> Int -> [Int]
                        alooper a cura resa
                          | a > lim = [resa, cura, b]
                          | 1 + a + b <= 0 = alooper (2 + a) cura resa
                          | resn > resa = alooper (2 + a) a resn
                          | otherwise = alooper (2 + a) cura resa
                          where resn :: Int
                                resn = nlooper 1 1
                                  where nlooper :: Int -> Int -> Int
                                        nlooper n res
                                          | is_prime ((n^2) + (a*n) + b) =
                                            nlooper (n+1) (1+res)
                                          | otherwise = res 






