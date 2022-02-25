module AoC2021.Day07
  ( aoc202107,
  )
where

import Data.List.Utils (split)
import Utils (readInt, (|>))

aoc202107 input = (part1, part2)
  where
    inputs = input |> split "\n" |> head |> split "," |> map readInt
    solve cost = [minimum inputs .. maximum inputs] |> map (\h -> inputs |> map (cost h) |> sum) |> minimum

    part1 = solve (\i h -> abs (i - h))
    part2 = solve (\i h -> let d = abs (i - h) in d * (d + 1) `div` 2)
