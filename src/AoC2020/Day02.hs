module AoC2020.Day02
( aoc202002a
, aoc202002b
, parseInput202002
) where

import qualified Data.Maybe as Maybe
import qualified Data.Map as M
import Utils

parseInput202002 input = map (parseTokens . words) $ lines input
    where parseTokens (r:cc:t:[]) =
           let a:b:x = map readInt $ words $ replace '-' ' ' r
           in ((a, b), head cc, t)

pred1 :: ((Int,Int), Char, String) -> Bool
pred1 ((b,e), c, s) = maybe False (\f -> b <= f && f <= e ) $ M.lookup c $ freq s

pred2 :: ((Int,Int), Char, String) -> Bool
pred2 ((b,e), c, s) = (s!!(b - 1) == c) /= (s!!(e - 1)  == c)

aoc202002a = length . filter pred1
aoc202002b = length . filter pred2
