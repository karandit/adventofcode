module AoC2021.Day01
  ( aoc202101,
  )
where

import Utils (readInt, (|>))

increasings nrs = zip nrs (nrs |> tail) |> filter (\(a1, a2) -> a2 > a1)

sumOfSlidings nrs = zip3 nrs (nrs |> tail) (nrs |> tail |> tail) |> map (\(a1, a2, a3) -> a1 + a2 + a3)

aoc202101 input = (part1, part2)
  where
    inputNrs = input |> lines |> map readInt

    part1 = inputNrs |> increasings |> length
    part2 = inputNrs |> sumOfSlidings |> increasings |> length
