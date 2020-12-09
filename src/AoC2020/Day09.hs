module AoC2020.Day09
( aoc202009a
, aoc202009b
) where

import Data.List (inits, tails, find)
import Utils (readInt)

aoc202009a preamble input =
    aoc202009a' preamble $ map readInt $ lines input

aoc202009a' preamble nums =
    let isNotPairSum xs goal= null $ [x*y | x:ys <- tails xs, y <- ys, x + y == goal]

        findNotPairSum acc@(pre, sol) n = case sol of
            Just _ -> acc
            Nothing -> if isNotPairSum pre n then ([], Just n) else ((tail pre) ++ [n], Nothing)

    in maybe 0 id $ snd $ foldl findNotPairSum (take preamble nums, Nothing) $ drop preamble nums

aoc202009b preamble input =
    let nums = map readInt $ lines input
        invalid = aoc202009a' preamble nums

        findContiguous xs = case find ((==) invalid . sum) $ inits xs of
          Just found -> found
          Nothing -> findContiguous $ tail xs

        contiguous = findContiguous nums
    in maximum contiguous + minimum contiguous 
