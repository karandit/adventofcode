module AoC2020.Day10
( aoc202010a
, aoc202010b
) where

import Data.List
import qualified Data.Map as M
import qualified Data.List.Utils as LL
import Utils

deltas input = 
    let ints = map readInt $ lines input
        nums = sort $ (3 + maximum ints):ints    
    in map fst . scanl (\(_, prev) i -> (abs (prev -i), i) ) (1,head nums) $ tail nums

ways 0 = 1
ways 1 = 2
ways 2 = 4
ways 3 = 7

aoc202010a input = product . M.elems . freq $ deltas input
aoc202010b input = 
    let ds = deltas input        
    in product . map (ways . length) . LL.split [True] . map (\(i,i') -> i==3 || i'==3) $ zip (init ds) (tail ds)
