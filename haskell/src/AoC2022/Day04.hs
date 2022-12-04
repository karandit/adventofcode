module AoC2022.Day04
  ( aoc202204,
  )
where

import Data.List (intersect)
import Data.List.Utils (split)
import Utils (readInt, (|>))

aoc202204 input = (part1, part2)
  where
    solve f =
      input
        |> lines
        |> map (split ",")
        |> map (\[a, b] -> (split "-" a |> map readInt, split "-" b |> map readInt))
        |> filter f
        |> length

    part1 =
      solve
        (\([a1, a2], [b1, b2]) -> (a1 >= b1 && a2 <= b2) || (b1 >= a1 && b2 <= a2))
    part2 =
      solve
        (\([a1, a2], [b1, b2]) -> intersect [a1 .. a2] [b1 .. b2] |> null |> not)
