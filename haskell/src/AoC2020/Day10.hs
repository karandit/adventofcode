module AoC2020.Day10
  ( aoc202010,
  )
where

import Data.List
import qualified Data.List.Utils as LL
import qualified Data.Map as M
import Utils

deltas input =
  let ints = map readInt $ lines input
      nums = sort $ (3 + maximum ints) : ints
   in map fst . scanl (\(_, prev) i -> (abs (prev - i), i)) (1, head nums) $ tail nums

ways 0 = 1
ways 1 = 2
ways 2 = 4
ways 3 = 7

aoc202010 input = (part1, part2)
  where
    part1 = product . M.elems . freq $ deltas input
    part2 =
      let ds = deltas input
       in product . map (ways . length) . LL.split [True] . map (\(i, i') -> i == 3 || i' == 3) $ zip (init ds) (tail ds)
