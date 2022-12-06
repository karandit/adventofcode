module AoC2022.Day06
  ( aoc202206,
  )
where

import Data.List (nub)
import Utils ((|>))

aoc202206 input = (part1, part2)
  where
    findMarker n s =
      [0 .. (length s - n)]
        |> filter (\i -> s |> drop i |> take n |> nub |> length |> (==) n)
        |> head
        |> (+) n

    part1 = input |> lines |> map (findMarker 4)
    part2 = input |> lines |> map (findMarker 14)
