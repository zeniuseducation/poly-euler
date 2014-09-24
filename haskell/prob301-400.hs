import Data.List
import Math

exp2 n
  | m == 1 && r == 0 = True
  | r /= 0 = False
  | otherwise = exp2 m
  where m = quot n 2
        r = rem n 2

specPrimes lim = [x|i <- [1.. (ceiling $ logBase 2 lim)], let x = 2^i + 1, prime x]

powerFor2 lim = [ m | m <- [1..lim], (rem m 2) == 0,
                  ((rem (succ m) 3) == 0) || ((rem (m + 3) 3) == 0) || ((rem (m +7) 3) == 0) ||
                  ((rem (m + 15) 3) == 0)]

useful2 lim = [m | m <- powerFor2 lim, 2^m < 10^10]

spPrimePow lim = [round res| p <- [5, 17, 257, 65537], i <- [1.. (ceiling (logBase 5 lim))],
                  let res = p^i , res < lim]


-- notes
-- pm => 2,4
-- p => 5,17
aForP :: (Integral a) => a -> a -> [a]
aForP pm lim = [m | m <- [2,4.. (ceiling $ logBase 2 (fromIntegral lim))],
                ((rem (m + pm - 1) 3) == 0)]

pPowers :: (Integral b) => b -> b -> [b]
pPowers p lim = [pn | n <- [4,6.. (ceiling $ logBase (fromIntegral p) (fromIntegral lim))],
                 ((rem (succ n) 3) == 0), let pn = p^n, pn < lim]

pByPm :: (Integral c) => c -> c
pByPm pm = (2^pm) + 1

nByPm :: (Integral d) => d -> d -> [d]
nByPm pm lim = [res | a <- (aForP pm lim), q <- (pPowers (succ (2^pm)) lim),
                let res = (2^a) * (((2^pm)+1)^q), res < lim]


