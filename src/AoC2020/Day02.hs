module AoC2020.Day02
( aoc202002
) where

import qualified Data.Maybe as Maybe
import qualified Data.Map as M
import Utils

pred1 :: ((Int,Int), Char, String) -> Bool
pred1 ((b,e), c, s) = maybe False (\f -> b <= f && f <= e ) $ M.lookup c $ freq s

pred2 :: ((Int,Int), Char, String) -> Bool
pred2 ((b,e), c, s) = (s!!(b - 1) == c) /= (s!!(e - 1)  == c)

-- | Part 1
-- >>> part1 [((1,3), 'a', "abcde")]
-- 1
-- >>> part1 [((1,3), 'b', "cdefg")]
-- 0
-- >>> part1 [((2,9), 'c', "ccccccccc")]
-- 1
part1 = length . filter pred1

-- | Part 2
-- >>> part2 [((1,3), 'a', "abcde")]
-- 1
-- >>> part2 [((1,3), 'b', "cdefg")]
-- 0
-- >>> part2 [((2,9), 'c', "ccccccccc")]
-- 0
part2 = length . filter pred2

aoc202002 input = (part1 parsedInput, part2 parsedInput) where
    parseTokens [r, cc, t] =
           let a:b:x = map readInt $ words $ replace '-' ' ' r
           in ((a, b), head cc, t)
    parsedInput = map (parseTokens . words) $ lines input
