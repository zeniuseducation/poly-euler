module Qmath where

import Clojure
import Data.List
import qualified Data.Set as Set
import Data.Ord


sqr n = n^2

primeHelper :: Int -> Int -> Bool
primeHelper p i
  | (i*i) > p = True
  | 0 == (rem p i) = False
  | otherwise = primeHelper p (i + 2)

-- it returns true if p is prime and false otherwise
prime' :: Int -> Bool
prime' p
  | p <= 10 = elem p [2,3,5,7]
  | even p = False
  | otherwise = primeHelper p 3


-- it returns the first positive primes greater than x
nextPrime :: Int -> Int
nextPrime x
  | x == 2 = 3
  | even x = if prime' $ succ x then succ x else nextPrime $ succ x
  | otherwise = if prime' $ 2 + x then 2 + x else nextPrime $ 2 + x


-- it returns the first positive primes less than x
prevPrime :: Int -> Int
prevPrime x
  | x <= 3 = 2
  | even x = if prime' $ pred x then pred x else prevPrime $ pred x
  | otherwise = if prime' $ x - 2 then x - 2 else prevPrime $ x - 2

-- it returns the sum of all primes under lim
sumPrimes :: Int -> Int
sumPrimes lim = helper 3 lim 2
  where helper i lim res
          | i >= lim = res
          | otherwise = helper (nextPrime i) lim (i + res)

-- sol10 x = sumPrimes 2000000
-- 0.42 sec on darklord
        
-- it stores the value of primes in memory for faster access next time
primes = iterate nextPrime 2

-- sol7 x = primes !! 10000
-- less than 10 ms on darklord

-- it returns all primes less than n
primesUnder :: Int -> [Int]
primesUnder n = takeWhile (n > ) primes

div' :: (Integral t) => t -> t -> Bool
div' a m = (0 == rem a m)

pfactors :: Int -> [Int]
pfactors n = helper 2 n []
  where helper p n res
          | prime' n = n:res
          | div' n p = helper 2 (quot n p) (p:res)
          | otherwise = helper (nextPrime p) n res

-- Problem no 3 using pfactors took less than 10msec
-- Problem no 7 using sieve took less than 1msec

palin' n = res == reverse res
  where res = numcol n

numcol :: (Integral a) => a -> [Int]
numcol n = helper n []
  where helper :: (Integral a) => a -> [Int] -> [Int]
        helper i res
          | i < 10 = (fromIntegral i) : res
          | otherwise = helper (quot i 10)
                        ((fromIntegral (rem i 10)) : res)

palin3x3 = maximum [x*y| x <- [900..999], y <- [900..999], palin' (x*y)]

-- sol9 = palin3x3

pita1000 = [a*b*c | a <- [1..250], b <- [a..500],
            let c = 1000 - b -a, a^2 + b^2 == c^2]
-- sol14??? pitagoras special
-- 0.02 sec on darklord

-- Problem no 9 using smart list comprehension 10ms
                                            
triangle :: Int -> Int
triangle n = quot (n * (succ n)) 2

divisors :: Int -> [Int]
divisors 1 = [1]
divisors 2 = [1,2]
divisors n
  | even n = helperEven n 3 [1,2,(div n 2),n]
  | otherwise = helperOdd n 3 [1,n]
  where helperOdd :: Int -> Int -> [Int] -> [Int]
        helperOdd n i res
          | (i*i) > n = res
          | div' n i = if i == div n i
                       then i : res
                       else helperOdd n (i + 2) (i : (div n i) : res)
          | otherwise = helperOdd n (i + 2) res
        helperEven :: Int -> Int -> [Int] -> [Int]
        helperEven n i res
          | (i * i) > n = res
          | div' n i = if i == div n i
                       then i : res
                       else helperEven n (succ i) (i: (div n i) : res)
          | otherwise = helperEven n (succ i) res

countDivs :: Int -> Int
countDivs 1 = 1
countDivs 2 = 2
countDivs n
  | even n = helperEven n 3 4
  | otherwise = helperOdd n 3 2
  where helperOdd :: Int -> Int -> Int -> Int
        helperOdd n i res
          | (i*i) > n = res
          | div' n i = if i == div n i
                       then succ res
                       else helperOdd n (i + 2) (2 + res)
          | otherwise = helperOdd n (i + 2) res
        helperEven :: Int -> Int -> Int -> Int
        helperEven n i res
          | (i * i) > n = res
          | div' n i = if i == div n i
                       then succ res
                       else helperEven n (succ i) (2 + res)
          | otherwise = helperEven n (succ i) res

firstTriangle :: Int -> (Int, Int)
firstTriangle lim = head $ dropWhile (\x -> (fst x) < lim) materials
  where materials = map (\x -> (countDivs x, x)) $ map triangle [1..]

-- Problem no 12 elapsed time 0.66 secs
-- 0.28 sec on dark lord

-- sol12 = firstTriangle 500

sumDivs :: Int -> Int
sumDivs 1 = 0
sumDivs 2 = 3
sumDivs n
  | even n = amicEven 
  | otherwise = amicOdd
  where amicEven = helperEven n 3 (2 + (div n 2) + 1)
        amicOdd = helperOdd n 3 1
        helperOdd :: Int -> Int -> Int -> Int
        helperOdd n i res
          | (i*i) > n = res
          | div' n i = if i == div n i
                       then i + res
                       else helperOdd n (i + 2) (i + (div n i) + res)
          | otherwise = helperOdd n (i + 2) res
        helperEven :: Int -> Int -> Int -> Int
        helperEven n i res
          | (i * i) > n = res
          | div' n i = if i == div n i
                       then i + res
                       else helperEven n (succ i) (i + (div n i) + res)
          | otherwise = helperEven n (succ i) res

amic' :: Int -> Bool
amic' n = (n == sumDivs amics) && n /= amics
  where amics = sumDivs n

-- Problem no 21 => secs 0.03

prob29 lim = Set.size $ Set.fromList [a^b| a <- [2..lim], b <- [2..lim]]
-- 0.05 sec on darklord

collatz :: Int -> (Int,Int)
collatz lim = maximumBy (comparing snd)  (map collas [1..lim])
  where collas :: Int -> (Int, Int)
        collas n = (n, colls n)
        colls :: Int -> Int
        colls n = if n == 1 then 1 else 1 + colls (calcol n)
        calcol :: Int -> Int
        calcol n = if even n then div n 2 else succ (3*n)

sol24 sx = (sort $ permutations "0123456789") !! sx        
-- 4.8 sec on dark lord

sol30 :: Int -> Int
sol30 x = sum (filter required' [1..x])
  where sumfif :: Int -> Int
        sumfif p = sum $ map (\x -> x^5) (numcol p)
        required' :: Int -> Bool
        required' i = (i == sumfif i)
-- 0.17 sec on darklord
        
colnum :: (Integral a) => [a] -> a
colnum [] = 0
colnum ls = helper ls 0
  where helper (x:[]) res = x + res*10
        helper (x:xs) res = helper xs (x + res*10)

pandigital' ls = ( [1..9] == sort ls)
 
sol32 lim = sum $ distinct
            [c| a <- [1..lim], b <- [1..lim],
             a /= b, let c = a*b,
             pandigital' $ (numcol a) ++ (numcol b) ++ (numcol c)]

fact :: (Integral a) => a -> a
fact 0 = 1
fact n = product [1..n]

fact' :: Int -> Bool
fact' n = (n == sum (map fact (numcol n)))

-- it returns all possible cycle of a number n
circulars :: Int -> [Int]
circulars n = map (colnum.cycleit) [0..pred $ length ncol]
  where ncol = numcol n
        cycleit i = (drop i ncol) ++ (take i ncol)

-- it returns true if n is a circular prime
circularPrime' :: Int -> Bool
circularPrime' n
  | filtered n = False
  | otherwise = all prime' $ circulars n
  where ncol = numcol n
        filtered n = any existlah [0,2,4,5,6,8]
        existlah l = elem l ncol

sol35 lim = 2 + (length $ filter circularPrime' $ primesUnder lim)
-- 0.55 in mba and 0.12 in darklord

-- it returns the binary representation of n in a list
bincol :: Int -> [Int]
bincol n = bhelper n []
  where bhelper n res
          | n < 2 = n : res
          | otherwise = bhelper (div n 2) ((rem n 2) : res)

-- it returns true if n is a palindrome in decimal and binary bases
palin'' :: Int -> Bool
palin'' n = ((ncol == reverse ncol) && (nbin == reverse nbin))
  where ncol = numcol n
        nbin = bincol n

sol36 lim = sum $ filter palin'' [1..lim]
-- elapsed time 0.77 sec for lim = 1 milion
-- 0.47 in darklord

tprime' n = all prime' $ ln ++ rn
  where ncol = numcol n
        ln = map colnum $ take (length ncol) $ iterate init ncol
        rn = map colnum $ take (length ncol) $ iterate tail ncol

sol37 start = sum $ take 11 $ filter tprime' (dropWhile (< start) primes)
-- elapsed time 0.7 sec
-- 0.12 sec in darklord

-- it returns true if n is a perfect square
psqr' :: Integral a => a -> Bool
psqr' n = ((ceiling nsqrt) == (floor nsqrt))
  where nsqrt = sqrt (fromIntegral n)

-- it returns the list of perimeters <= lim with which it is possible to construct an
-- all-integer sided right triangle. 
pitas :: Int -> [Int]
pitas lim = [a + b + c | a <- [3..(div lim 4)],
             b <- [(succ a)..(limb $ fromIntegral a)],
             let csqr = ((a^2) + (b^2)), (psqr' csqr),
             let c = (round (sqrt $ fromIntegral csqr)), (a+b+c) <= lim]
  where limb i = minimum [(div lim 2),(ceiling ((succ (i^2)) / 2))]

sol39 lim = last $ csortBy snd $ frequencies $ pitas lim

int' :: RealFrac p => p -> Bool
int' m = m == (fromIntegral (round m))

sol75 lim = length $ filter (\x -> (snd x) == 1) $ frequencies $ pitas lim


sol40 lim = product $ map (\x -> (concatMap numcol [1..]) !! (pred x))
            $ map (10^) [0..lim]


bprimeHelper :: Integer -> Integer -> Bool
bprimeHelper p i
  | (i*i) > p = True
  | 0 == (rem p i) = False
  | otherwise = bprimeHelper p (i + 2)

-- it returns true if p is prime and false otherwise
bprime' :: Integer -> Bool
bprime' p
  | p <= 10 = elem p [2,3,5,7]
  | even p = False
  | otherwise = bprimeHelper p 3

-- it returns the first positive primes greater than x
bnextPrime :: Integer -> Integer
bnextPrime x
  | x == 2 = 3
  | even x = if bprime' $ succ x then succ x else bnextPrime $ succ x
  | otherwise = if bprime' $ 2 + x then 2 + x else bnextPrime $ 2 + x

bpfactors :: Integer -> [Integer]
bpfactors n = helper 2 n []
  where helper p n res
          | bprime' n = n:res
          | div' n p = helper 2 (quot n p) (p:res)
          | otherwise = helper (bnextPrime p) n res

combs :: Integer -> Integer -> Integer
combs n k = div (product [(succ res)..n]) (product [1..(n - res)])
  where res = maximum [(n - k),k]

sol231 n k = bpfactors $ combs n k

nonBouncyDigit n = 2 * (triangle n)

primesFrom1Billion = iterate prevPrime 987654319

sol41 x = maximum $ filter prime' $ map colnum $ permutations [1..x]

substringDiv' i (x:xs)
  | (length xs) < 3 = True
  | otherwise = if div' (colnum $ take 3 xs) i
                then substringDiv' (nextPrime i) xs 
                else False

sol43 = sum $ map colnum $ filter (substringDiv' 2) $ permutations [0..9]

abs x
  | x >= 0 = x
  | otherwise = -x

sol48 :: Integral a => a -> a -> a
sol48 1000 res = res
sol48 i res = sol48 (succ i) (rem (res + (i^i)) (10^10))

bouncy i 1 = 1
bouncy i 2 = 10 - i
bouncy i 3 = div ((10-i) * (i+1)) 2
bouncy i n = sum $ map (\x -> bouncy x (pred n)) [i..9]

sol113 lim = sum [bouncy i lim| i <- [1..9]]


zl1a 1 = 1
zl1a n = n + (zl1a $ pred n)

zl1b 1 = 1
zl1b n = div (n * (succ n)) 2

trimolist 1 = [1]
trimolist 2 = [1,1]
trimolist 3 = [1,1,1]
trimolist n = helper 3 [1,1,1]
  where helper i (x:y:z:xs)
          | i == n = (x:y:z:xs)
          | otherwise = helper (succ i) (x+y+z:x:y:z:xs)

fibo i = fibolist 2 [1,1]
  where fibolist n (x:y:_)
          | n == i = x
          | otherwise = fibolist (succ n) (x+y: [x])

zl20 i = gen_fibo 2 [1,1]
  where gen_fibo n (x:y:_)
          | x >= i = (n,x)
          | otherwise = gen_fibo (succ n) (x+y: [x])

fibolist 1 = [1]
fibolist i = reverse $ fibo_help 2 [1,1]
  where fibo_help n (x:y:xs)
          | n == i = (x:y:xs)
          | otherwise = fibo_help (succ n) (x+y:x:y:xs)

fibo_under 1 = []
fibo_under 2 = [1,1]
fibo_under i = reverse $ fibo_help [1,1]
  where fibo_help (x:y:xs)
          | x >= i = (y:xs)
          | otherwise = fibo_help (x+y:x:y:xs)

numcol4 n
  | n < 4 = [n]
  | otherwise = numcol4 (div n 4) ++ [(rem n 4)]

bas4_to_dec n = helper (numcol n) 0
  where helper (x:xs) res
          | null xs = x + (4 * res)
          | otherwise = helper xs (x + (4*res))

dec_to_bas4 n = numcol4 n

permute n k = product [(succ (n-k))..n]

pascal_row (x:xs) res
  | null xs = x:res
  | otherwise = pascal_row xs (x+ (head xs) :res)

pascal row = pascal_row row [1]

nth_pascal n = (iterate pascal [1,1]) !! (pred n)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

binom a b n = map (\x -> (fst3 x) * (snd3 x) * (trd3 x)) result
  where result = zip3 (nth_pascal n)
                 (map (a^) [n,(pred n)..0])
                 (map (b^) [0..n])

zl29 = sum $ binom 1 3 10

zl30 = sum [(product [9,19]), (product [9,19,26]), (product [9,10,19]),
            (product [9,10,19,26]), (product [9,10,10,19]), (product [9,10,10,19,26]),
            (product [9,10,10,10, 19]), product ([9,10,10,10,19,26])]

countDigits lim = length $ concatMap numcol [1..lim]

countDigs_helper 0 = 0
countDigs_helper 1 = 9
countDigs_helper n = 9 * n * (10^ (pred n))

countDigs_base n = sum $ map countDigs_helper [1..n]

countDigs lim = countDigs_base (pred n) + remainder n
  where n = length $ numcol lim
        remainder i = ((succ lim) - (10^ (pred i))) * i

product' [] = 1
product' (x:xs) = x * product' xs

