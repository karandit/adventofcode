module AoC2015.Day01
( aoc201501
) where

import Data.Maybe (fromMaybe)
import Data.List (findIndex)

direction '(' = 1
direction ')' = -1
direction  _  = 0

-- | Part1
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

-- | Part2
--
-- >>> part2 ")"
-- 1
-- >>> part2 "()())"
-- 5
part2 :: String -> Int
part2 = fromMaybe (-500) . findIndex (< 0) . scanl (+) 0 . map direction

aoc201501 :: String -> (Int, Int)
aoc201501 input = (part1 input, part2 input)
