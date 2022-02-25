module AoC2015.Day01
  ( aoc201501,
  )
where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

direction '(' = 1
direction ')' = -1
direction _ = 0

-- | Part 1
--
-- >>> part1 "(())"
-- 0
-- >>> part1 "()()"
-- 0
-- >>> part1 "(()(()("
-- 3
-- >>> part1 "))((((("
-- 3
-- >>> part1 "))("
-- -1
-- >>> part1 ")())())"
-- -3
part1 :: String -> Int
part1 input = sum . map direction $ input

-- | Part 2
--
-- >>> part2 ")"
-- 1
-- >>> part2 "()())"
-- 5
part2 :: String -> Int
part2 = fromMaybe (-500) . findIndex (< 0) . scanl (+) 0 . map direction

aoc201501 :: String -> (Int, Int)
aoc201501 input = (part1 input, part2 input)
