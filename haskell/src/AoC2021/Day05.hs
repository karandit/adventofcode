module AoC2021.Day05
  ( aoc202105,
  )
where

import qualified Data.List.Utils as LL
import qualified Data.Map as M
import Utils (freq, readInt, (|>))

aoc202105 input = (part1, part2)
  where
    ventures = input |> lines |> map parseVent
    parseVent vent =
      vent
        |> LL.split " -> "
        |> map (\coords -> coords |> LL.split "," |> map readInt |> \(f1 : f2 : []) -> (f1, f2))
        |> \(f1 : f2 : []) -> (f1, f2)

    part1Conv ((x1, y1), (x2, y2)) = [(x, y) | x <- [(min x1 x2) .. (max x1 x2)], y <- [(min y1 y2) .. (max y1 y2)]]
    part1 =
      ventures
        |> filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)
        |> map part1Conv
        |> concat
        |> freq
        |> M.toList
        |> filter (\(_, fr) -> fr > 1)
        |> length

    part2Conv ((x1, y1), (x2, y2)) = zip [x1, x1 + (signum $ x2 - x1) .. x2] [y1, y1 + (signum $ y2 - y1) .. y2]
    part2 =
      ventures
        |> map (\c@((x1, y1), (x2, y2)) -> if (x1 == x2 || y1 == y2) then part1Conv c else part2Conv c)
        |> concat
        |> freq
        |> M.toList
        |> filter (\(_, fr) -> fr > 1)
        |> length
