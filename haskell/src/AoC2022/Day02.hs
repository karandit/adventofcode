module AoC2022.Day02
  ( aoc202202,
  )
where

import Utils ((|>))

aoc202202 input = (part1, part2)
  where
    parseC1 'A' = 1
    parseC1 'B' = 2
    parseC1 'C' = 3

    parseC2 'X' = 1
    parseC2 'Y' = 2
    parseC2 'Z' = 3

    whatb 1 3 = 2
    whatb 2 3 = 3
    whatb 3 3 = 1
    whatb 1 1 = 3
    whatb 2 1 = 1
    whatb 3 1 = 2
    whatb a 2 = a

    solution f = input |> lines |> map (\[a, ' ', b] -> f (parseC1 a) (parseC2 b)) |> sum

    calc1 a b = b + points
      where
        dwl = (3 + b - a) `mod` 3 -- 0 draw, 1 win, 2 lose
        points = ((1 + dwl) `mod` 3) * 3 -- 3 points draw, 6 points win, 0 points lose
    calc2 a ldw = b + points
      where
        b = whatb a ldw -- 1 lose, 2 draw, 3 win
        points = (ldw - 1) * 3 -- it is the same as in calc1
    part1 = solution calc1
    part2 = solution calc2
