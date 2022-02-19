module AoC2020.Day09
( aoc202009
) where

import Data.List (inits, tails, find)
import Utils (readInt)

part1 preamble input =
    part1' preamble $ map readInt $ lines input

part1' preamble nums =
    let isNotPairSum xs goal= null $ [x*y | x:ys <- tails xs, y <- ys, x + y == goal]

        findNotPairSum acc@(pre, sol) n = case sol of
            Just _ -> acc
            Nothing -> if isNotPairSum pre n then ([], Just n) else ((tail pre) ++ [n], Nothing)

    in maybe 0 id $ snd $ foldl findNotPairSum (take preamble nums, Nothing) $ drop preamble nums

part2 preamble input =
    let nums = map readInt $ lines input
        invalid = part1' preamble nums

        findContiguous xs = case find ((==) invalid . sum) $ inits xs of
          Just found -> found
          Nothing -> findContiguous $ tail xs

        contiguous = findContiguous nums
    in maximum contiguous + minimum contiguous 

aoc202009 preamble input = (part1 preamble input, part2 preamble input)
