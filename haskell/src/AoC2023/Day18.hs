module AoC2023.Day18
( aoc202318
) where

import Utils (readInt, (|>))
import Data.Foldable (foldl')
import Numeric (readHex)

-- | Area of a polygon using Shoelace formula
shoelace ps = zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - x2 * y1) (init ps) (tail ps) |> sum |> abs |> \x -> x `quot` 2

-- | Pick's theorem: area = interior + boundary/2 - 1
-- interior = area - boundary/2 + 1
-- lava = interior + boundary = area + boundary/2 + 1
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
    area = shoelace ps
    in area + 1 + boundary `quot` 2

  mapDir '0' = 'R'
  mapDir '1' = 'D'
  mapDir '2' = 'L'
  mapDir '3' = 'U'

  instrs2 = inputs |> map (\s -> s |> words |> \[_, _, c] -> c |> drop 2 |> \h6 -> (mapDir $ head $ drop 5 h6, fst $ head $ readHex $ take 5 h6))
  part1 = solve instrs1
  part2 = solve instrs2
