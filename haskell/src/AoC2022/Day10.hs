module AoC2022.Day10
  ( aoc202210,
  )
where

import Utils (readInt, chunksOf, (|>))

data Cmd = Addx Int | NoOp deriving (Show)

aoc202210 input = (part1, part2)
  where
    parse ('a' : 'd' : 'd' : 'x' : ' ' : unparsed) = Addx (readInt unparsed)
    parse _ = NoOp

    proc (x, xs) NoOp = (x, x : xs)
    proc (x, xs) (Addx n) = (x + n, x : x : xs)

    inputs = input |> lines |> map parse
    values = inputs |> foldl proc (1, []) |> snd |> reverse
    part1 = [20, 60 .. length values] |> map (\i -> i * (values !! (i -1))) |> sum
    part2 =
      values |> zip [0 ..]
        |> map (\(i, x) -> let crt = i `mod` 40 in if crt >= x -1 && crt <= x + 1 then '#' else '.')
        |> chunksOf 40
