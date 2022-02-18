module AoC2020.Day03 
( aoc202003
) where

import Utils

countTrees left down =
  let slope (col, row, count) trees =
        if row `mod` down == 0
        then ((col + left) `mod` (length trees), row + 1, count + (trees!!col == '#' ? 1 :? 0))
        else (col, row + 1, count)
  in foldl slope (left, down, 0)

aoc202003 input = (part1, part2) where
  rows = tail $ lines input
  (_, _, res3) = countTrees 3 1 rows
  part1 = res3

  (_, _, res1) = countTrees 1 1 $ rows
  (_, _, res5) = countTrees 5 1 $ rows
  (_, _, res7) = countTrees 7 1 $ rows
  (_, _, res2) = countTrees 1 2 $ tail rows
  part2 = res1 * res3 * res5 * res7 * res2
