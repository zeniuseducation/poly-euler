module One where

import Data.List


numcol :: Int -> [Int]
numcol n = looper n []
  where looper :: Int -> [Int] -> [Int]
        looper i res
          | i < 10 = i : res
          | otherwise = looper (div i 10) ((rem i 10):res)

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
  | otherwise = outer 3
  where outer :: Int -> Bool
        outer i
          | i*i > n = True
          | 0 == rem n i = False
          | otherwise = outer (i+2)

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

refs5 = [0,1,2^5,3^5,4^5,5^5,6^5,7^5,8^5,9^5]

is_sumfif :: Int -> Bool
is_sumfif n = looper n 0
  where looper :: Int -> Int -> Bool
        looper i res
          | i < 10 = n == res + (i^5)
          | res > n = False
          | otherwise = looper (div i 10) (res + (rem i 10)^5)

euler30 :: Int -> Int 
euler30 lim = looper lim 0
  where looper :: Int -> Int -> Int
        looper i res
          | i == 10 = res
          | is_sumfif i = looper (i-1) (res+i)
          | otherwise = looper (pred i) res

coins = [1,2,5,10,20,50,100,200]

changes n = looper n 7
  where looper i c
          | i == 0 = 1
          | c == 0 = 1
          | otherwise = result
          where result = foldl1 (+) possibles
                possibles = map (\x -> looper (i - (x * (coins !! c))) (c - 1)) cosi
                cosi = takeWhile (\x -> x * (coins !! c) <= i) [0..]

sum_ints :: Int -> Int
sum_ints n = looper n (pred n)
  where looper :: Int -> Int -> Int
        looper i c
          | i == 0 = 1
          | c == 1 = 1
          | otherwise = inner 0 0
          where inner :: Int -> Int -> Int
                inner x res
                  | i < x*c = res
                  | otherwise = inner (succ x) (res + (looper (i - (x*c)) (c-1)))

colnum :: [Int] -> Int
colnum ls = looper ls 0
  where looper :: [Int] -> Int -> Int
        looper [] res = res
        looper (x:xs) res = looper xs (10*res + x)


pandig_prime n = looper (n-1)
  where raw = reverse [1..n]
        looper i
          | i == 0 = 0 
          | null res = looper (i-1)
          | otherwise = maximum res
          where res = filter is_prime res1
                res1 = map (\x -> colnum $ (take i raw) ++ x)
                       (permutations $ drop i raw)

-- returns true if ls is pandigital 
is_pandig :: [Int] -> Bool
is_pandig ls = (sort ls) == [1..9]

-- returns the sum of all pandigital products
pandig_products :: Int -> Int
pandig_products lim = sum $ nub $ outer 2 []
  where outer :: Int -> [Int] -> [Int]
        -- returns the list of pandigital products
        outer i res
          | i*i > lim = res
          | otherwise = outer (succ i) (res ++ (inner (succ i) []))
          where inner :: Int -> [Int] -> [Int]
                -- inner loop for j 
                inner j resj
                  | i*j > 3*lim = resj
                  | is_pandig result = inner (succ j) (i*j:resj)
                  | otherwise = inner (succ j) resj
                  where result :: [Int]
                        result = (numcol i) ++ (numcol j) ++ (numcol $ i * j)

fact i = product [1..i]

euler34 :: Int -> Int
euler34 lim = sum $ filter
              (\x-> x == (sum $ map (\y -> fact y) (numcol x))) [10..lim]

is_cprime :: Int -> Bool
is_cprime n
  | n < 10 = elem n [3,7]
  | otherwise = looper n res
  where res :: Int
        res = pred $ length $ numcol n
        looper :: Int -> Int -> Bool
        looper m i
          | (i== -1) = True
          | is_prime m = looper result (pred i)
          | otherwise = False
          where result = (div m 10) + ((rem m 10) * (10^res))

all_cprimes :: Int -> Int
all_cprimes lim = foldl (+) 2 (map looper bahan)
  where bahan = [1,3,7,9]
        looper :: Int -> Int
        looper i
          | i > lim = 0
          | is_cprime i = succ res
          | otherwise = res
          where res = sum $ map (\x -> looper $ 10*i + x) bahan

is_bipalin :: Int -> Bool
is_bipalin n = bcol == reverse bcol
  where bcol = bincol n []
        bincol :: Int -> [Int] -> [Int] 
        bincol i res
          | i < 2 = i:res
          | otherwise = bincol (div i 2) ((rem i 2):res)

sum_bipalins :: Int -> Int
sum_bipalins i
  | i <= 1 = sum [1,3..9]
  | even i = (evenpals (10^ (pred (div i 2))) 0) + (sum_bipalins $ pred i)
  | otherwise = (oddpals (10^ (pred (div i 2))) 0) + (sum_bipalins $ pred i)
  where evenpals :: Int -> Int -> Int
        evenpals n res
          | n >= (10 ^ (div i 2)) = res
          | even x = evenpals (colnum (succ x : xs)) res
          | is_bipalin tnum = evenpals (succ n) (tnum+res)
          | otherwise = evenpals (succ n) res
          where (x:xs) = numcol n
                tnum = colnum ((x:xs) ++ (reverse (x:xs)))
        oddpals :: Int -> Int -> Int 
        oddpals n res
          | n >= (10^ (div i 2)) = res
          | even x = oddpals (colnum ((succ x):xs)) res
          | otherwise = oddpals (succ n) (res + result)
          where result = sum $ filter is_bipalin res2
                res2 = map (\k -> colnum $ (x:xs) ++ [k] ++ (reverse (x:xs))) [0..9]
                (x:xs) = numcol n
                
pfactors :: Int -> [Int]
pfactors n = looper 2 n []
  where looper :: Int -> Int -> [Int] -> [Int]
        looper i divs res
          | is_prime divs = divs:res
          | 0 == rems = looper 2 (div divs i) (i:res)
          | otherwise = looper (next_prime i) divs res
          where rems = rem divs i

count_divs :: Int -> Int
count_divs n 
  | 0 == rem n 2 = looper 2 1 2
  | otherwise = looper 3 2 2
  where looper :: Int -> Int -> Int -> Int
        looper i j res
          | i*i >= n = if i*i == n then 1+res else res
          | 0 == rem n i = looper (j + i) j (res + 2)
          | otherwise = looper (j + i) j res

divs_count :: Int -> Int
divs_count n = product $ map (succ.length) (group $ pfactors n)

triangle_factors :: Int -> Int
triangle_factors lim = looper 2
  where looper :: Int -> Int
        looper i
          | res i > lim = (succ i) * (div i 2)
          | otherwise = looper $ succ i
          where res :: Int -> Int
                res n
                  | even n = (count_divs (div n 2)) * (count_divs (succ n))
                  | otherwise = (count_divs (div (succ n) 2)) * (count_divs n)

camps :: Int -> Int
camps n = foldl1 (*) $ map ((concatMap numcol $ iterate succ 1) !!) [10^i-1| i <- [0..n-1]]



