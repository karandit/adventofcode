module AoC2023.Day06
( aoc202306
) where

import Utils (readInt, (|>))
import Data.Char (isDigit)
import Data.List.Utils (split)

aoc202306 input = (part1, part2) where
  ways (t, d) = length [i | i <- [1..t], i * (t -i) > d]

  inputs1 = input |> lines
           |> map (\s -> s |> split ":" |> last |> words |> map readInt)
           |> (\[ts, ds] -> zip ts ds)
  part1 = inputs1 |> map ways |> product

  inputs2 = input |> lines
           |> map (\s -> s |> split ":" |> last |> filter isDigit |> readInt)
           |> (\[t, d] -> (t, d))
  part2 = ways inputs2
