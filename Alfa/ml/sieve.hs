module Sieve where

import Data.Array.IO
import qualified Data.Vector as V

sieve lim = do arr <- newArray (1,lim) 0 :: IO (IOArray Int Int)
               ars <- iter 1 lim arr
               sums <- outer 1 lim ars 0
               print sums
               where iter i j refs
                       | i == j = do return refs
                       | otherwise = do writeArray refs i i
                                        iter (i+1) j refs
                     outer i j refs res
                       | i == j = do return res
                       | otherwise = do tmp <- readArray refs i
                                        outer (i+1) j refs tmp

sumsieve lim = do refs <- newArray (1,lim) :: IO (IOArray Int Int)
                  let sums = outer 3 2 refs
                  print sums
                    where llim = ceiling.sqrt.fromIntegral $ lim
                          outer i res resi
                            | i >= lim = res
                            | refsi = do nresi <- inner (i*i) resi
                                         outer (i+2) (i+res) nresi
                            | otherwise = outer (i+2) res resi
                           where refsi = do return readArray resi i
                                 inner j resj
                                   | j >= lim = do return resj
                                   | otherwise = do writeArray resj j False
                                                    inner (j+i+i) resj
