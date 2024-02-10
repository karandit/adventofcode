module AoC2023.Day11
( aoc202311
) where

import Utils (readInt, manhattan2d, (|>))
import Data.List (tails)
import Data.Foldable (foldl')
import qualified Data.Map as M
import qualified Data.Set as S

pairs xs = [(h,r) | h:rs <- [t | t <- tails xs, length t > 1], r <- rs]

aoc202311 input = (part1, part2) where
  inputs = input |> lines
  image = M.fromList [ ((y, x), v) | (y, row) <- zip [0..] inputs, (x, v) <- zip [0..] row ] 
  maxY = length inputs - 1  
  maxX = length (head inputs) - 1
  galaxies = image |> M.filter (\c -> c == '#') |> M.keys
  ys = foldl' (\acc (y, _) -> S.delete y acc) ([0..maxY] |> S.fromList) galaxies
  xs = foldl' (\acc (_, x) -> S.delete x acc) ([0..maxX] |> S.fromList) galaxies

  solve expansion = let 
    expgalaxies = map (\(y, x) ->
            ( y + expansion * (S.size $ S.filter (\i -> i < y) ys)
            , x + expansion * (S.size $ S.filter (\i -> i < x) xs) )) galaxies
    in sum [manhattan2d g1 g2 | (g1, g2) <- pairs expgalaxies]

  part1 = solve 1
  part2 = solve 999999
