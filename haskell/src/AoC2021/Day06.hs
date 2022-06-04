module AoC2021.Day06
  ( aoc202106,
  )
where

import Data.List (foldl')
import Data.List.Utils (split)
import qualified Data.Map as M
import Utils (applyN, freq, readInt, (|>))

aoc202106 input = (part1, part2)
  where
    fishesByAges = input |> split "\n" |> head |> split "," |> map readInt |> freq
    fishes = [0 .. 8] |> foldl' (\acc k -> M.insertWith (+) k 0 acc) fishesByAges |> M.elems

    solve days = fishes |> applyN days (\[a0, a1, a2, a3, a4, a5, a6, a7, a8] -> [a1, a2, a3, a4, a5, a6, a7 + a0, a8, a0]) |> sum

    part1 = solve 80
    part2 = solve 256
