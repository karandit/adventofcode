module AoC2022.Day03
( aoc202203
) where

import Utils ((|>), chunksOf)
import Data.List (intersect)
import Data.Char (ord)

aoc202203 input = (part1, part2) where
    solve f = input 
            |> lines 
            |> f
            |> map (\ls-> foldl1 intersect ls |> head |> ord)
            |> map (\x -> if x < 97 then x - 38 else x - 96)
            |> sum
    part1 = solve (map (\l -> let half = (length l) `div` 2 in [take half l, drop half l]))
    part2 = solve (chunksOf 3)
