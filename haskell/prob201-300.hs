import Data.List
import Numeric
import Math

pfactorsHelper p n res
  | d /= 1 && r == 0 = if prime d then distinct (p:d:res) else pfactorsHelper 2 d (p:res)
  | d == 1 && r == 0 = distinct (p:res)
  | otherwise = pfactorsHelper (nextPrime p) n res
  where r = rem n p
        d = div n p

pfactors n = pfactorsHelper 2 n []

rtotient n = (fromIntegral n) *
             (product $ map (\x -> (fromIntegral 1) -
                                   (toRational (1 / (fromIntegral x)))) pfacts)
  where pfacts = pfactors n

resil n = (rtotient n) / (fromIntegral (pred n))

target = (toRational 15499/94744)
usePrimes = primesUnder 25
baseNum = product usePrimes
rawMat = subsequences usePrimes

step243 lim (x:xs) num
  | (fromRat (resil res)) < (fromRat lim) = [x,res]
  | otherwise = step243 lim xs num 
  where res = num * x

sol243 lim = step243 lim (distinct (map product rawMat)) baseNum 


