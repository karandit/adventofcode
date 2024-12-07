module AoC2024.Day04
( aoc202404
) where

import Utils (readInt, (|>))
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

aoc202404 input = (part1, part2) where
  inputs = input |> lines
  maxY = inputs |> length |> pred
  maxX = inputs |> head |> length |> pred
  grid = M.fromList [((y, x), v) | (y, row) <- zip [0..] inputs, (x, v) <- zip [0..] row] 

  horiz = [[(y,     x + d) | d <- [0..3]] | x <- [0..maxX-3], y <- [0..maxY  ]]
  verti = [[(y + d, x    ) | d <- [0..3]] | x <- [0..maxX  ], y <- [0..maxY-3]]
  dialr = [[(y + d, x + d) | d <- [0..3]] | x <- [0..maxX-3], y <- [0..maxY-3]]
  diarl = [[(y - d, x + d) | d <- [0..3]] | x <- [0..maxX-3], y <- [3..maxY  ]]

  predPart1 xmas = xmas == "XMAS" || xmas == "SAMX"

  solve f directions = directions |> map (length . filter (\coords -> f $ Maybe.catMaybes [ M.lookup yx grid | yx <- coords])) |> sum

  part1 = solve predPart1 [horiz, verti, dialr, diarl]

  predPart2 mas = mas == "MAS" || mas == "SAM"

  part2 = length $ filter id [
          (predPart2 $ Maybe.catMaybes [M.lookup (y+d, x+d) grid | d <- [-1..1]]) &&
          (predPart2 $ Maybe.catMaybes [M.lookup (y+d, x-d) grid | d <- [-1..1]])
          | x <- [1..maxX-1], y <- [1..maxY-1]]
