module AoC2020.Day01
  ( aoc202001,
  )
where

import Data.List (tails)

-- | Part 1
-- >>> part1 [1721, 979, 366, 299, 675, 1456]
-- 514579
part1 nums = head $ [x * y | x : ys <- tails nums, y <- ys, x + y == 2020]

-- | Part 2
-- >>> part2 [1721, 979, 366, 299, 675, 1456]
-- 241861950
part2 nums = head $ [x * y * z | x : ys <- tails nums, y : zs <- tails ys, z <- zs, x + y + z == 2020]

aoc202001 input = (part1 nums, part2 nums)
  where
    nums = map (read :: String -> Int) $ lines input
