module AoC2015.Day02
  ( aoc201502,
  )
where

import Utils

parseInput =
  let readNums = map readInt . words . replace 'x' ' '
   in map readNums . lines

-- | Part 1
-- >>> paper [2, 3, 4]
-- 58
-- >>> paper [1, 1, 10]
-- 43
paper [a, b, c] =
  let surfaces = [a * b, a * c, b * c]
   in 2 * (sum surfaces) + minimum surfaces

-- | Part 2
-- >>> ribbon [2, 3, 4]
-- 34
-- >>> ribbon [1, 1, 10]
-- 14
ribbon ns@[a, b, c] =
  let perimeters = [a + b, a + c, b + c]
   in 2 * (minimum perimeters) + product ns

aoc201502 input = (part1, part2)
  where
    triples = parseInput input
    part1 = sum $ map paper triples
    part2 = sum $ map ribbon triples
