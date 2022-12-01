module AoC2022.Day01
  ( aoc202201,
  )
where

import Data.List (sort)
import Data.List.Utils (split)
import Utils (readInt, (|>))

aoc202201 input = (part1, part2)
  where
    inputs = input |> lines |> split [""] |> map (sum . map readInt)

    part1 = inputs |> maximum
    part2 = inputs |> sort |> reverse |> take 3 |> sum
