module AoC2020.Day09
  ( aoc202009,
  )
where

import Data.List (find, inits, tails)
import Utils (readInt)

{- | Part 1
>>> part1 5 "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"
127
-}
part1 preamble input =
  part1' preamble $ map readInt $ lines input

part1' preamble nums =
  let isNotPairSum xs goal = null $ [x * y | x : ys <- tails xs, y <- ys, x + y == goal]

      findNotPairSum acc@(pre, sol) n = case sol of
        Just _ -> acc
        Nothing -> if isNotPairSum pre n then ([], Just n) else ((tail pre) ++ [n], Nothing)
   in maybe 0 id $ snd $ foldl findNotPairSum (take preamble nums, Nothing) $ drop preamble nums

{- | Part 2
>>> part2 5 "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"
62
-}
part2 preamble input =
  let nums = map readInt $ lines input
      invalid = part1' preamble nums

      findContiguous xs = case find ((==) invalid . sum) $ inits xs of
        Just found -> found
        Nothing -> findContiguous $ tail xs

      contiguous = findContiguous nums
   in maximum contiguous + minimum contiguous

aoc202009 input = (part1 25 input, part2 25 input)
