import Math
import Data.List

psqr :: Int -> Bool
psqr n = floor x == ceiling x
  where x = sqrt . fromIntegral $ n

perfects = [[a,b,c]| c <- [1..], b <- [(succ c)..], a <- [(succ b)..],
            psqr (a+b) && psqr (a-b) && psqr (a+c) && psqr (a-c) && psqr (b+c) && psqr (b-c)]

