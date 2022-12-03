module AoC2022.Day02
  ( aoc202202,
  )
where

import Data.Char (ord)
import Utils ((|>))

aoc202202 input = (part1, part2)
  where
    process f =
      input
        |> lines
        |> map (\[a, ' ', b] -> f (1 + ord a - ord 'A') (1 + ord b - ord 'X'))
        |> sum

    part1 =
      process
        ( \a b ->
            let dwl = (3 + b - a) `mod` 3 -- 0 draw, 1 win, 2 lose
                points = ((1 + dwl) `mod` 3) * 3 -- 3 points draw, 6 points win, 0 points lose
             in b + points
        )

    part2 =
      process
        ( \a ldw ->
            let guessb 1 1 = 3
                guessb a 1 = a - 1
                guessb a 2 = a -- draw
                guessb a 3 = (a `mod` 3) + 1 -- win
                points = (ldw - 1) * 3 -- it is the same as in calc1
             in guessb a ldw + points
        )
