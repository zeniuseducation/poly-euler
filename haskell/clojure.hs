module Clojure where

import Data.List
import qualified Data.Set as Set
import Data.Ord

distinct ls = map ( head . group . sort ) $ ls

frequencies ls = map (\x -> (head x, length x)) $ (group.sort) ls

sample = [1,1,1,1,2,3,4,5,6,7,8,9,0,7,5,3,4,2,5,64,2,1]

csortBy f ls = sortBy (\x y -> compare (f x) (f y)) ls

cgroupBy f ls = map (\x -> ((fst (head x)), (map snd x))) tmp3
  where tmp3 = groupBy (\x y -> (fst x) == (fst y)) tmp2
        tmp2 = csortBy fst tmp1
        tmp1 = map (\x -> (f x, x)) ls

