module AoC2023.Day18
( aoc202318
) where

import Utils (readInt, areaShoelace, (|>))
import Data.Foldable (foldl')
import Numeric (readHex)

aoc202318 input = (part1, part2) where
  inputs = input |> lines

  instrs1 = inputs |> map (\s -> s |> words |> \[d, n, _] -> (head d, readInt n))

  step (x, y) (dir, n) = case dir of
    'R' -> (x + n, y)
    'L' -> (x - n, y)
    'D' -> (x, y + n)
    'U' -> (x, y - n)

  solve instrs =  let
    boundary = instrs |> map snd |> sum
    ps = foldl' (\coords@(curCoord:_) instr -> (step curCoord instr):coords) [(0, 0)] instrs
    area = areaShoelace ps
    in area + 1 + boundary `quot` 2 -- lava = interior + boundary = area + boundary/2 + 1, see Pick's theorem

  mapDir '0' = 'R'
  mapDir '1' = 'D'
  mapDir '2' = 'L'
  mapDir '3' = 'U'

  instrs2 = inputs |> map (\s -> s |> words |> \[_, _, c] -> c |> drop 2 |> \h6 -> (mapDir $ head $ drop 5 h6, fst $ head $ readHex $ take 5 h6))
  part1 = solve instrs1
  part2 = solve instrs2
