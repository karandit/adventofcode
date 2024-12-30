module AoC2024.Day08
( aoc202408
) where

import Utils (add2d, neg2d, (|>))
import Data.List (tails)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do 
  input <- readFile "../inputs/2024/day08.txt"
  input <- readFile "../inputs/2024/day08S.txt"
  putStrLn $ show $ aoc202408 input

aoc202408 input = (part1, part2) where
  inputs = input |> lines
  maxY = inputs |> length |> pred
  maxX = inputs |> head |> length |> pred
  inRange (y, x) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

  antennasByFreq = M.elems $ M.fromListWith (++) [(v, [(y, x)]) | (y, row) <- zip [0..] inputs, (x, v) <- zip [0..] row, v /= '.'] 

  antinodes mkNodes ants = concat [ mkNodes p1 p2 d | p1@(y1, x1): rs <- tails ants, p2@(y2, x2) <- rs, let d = (y1 - y2, x1 - x2)]

  mkPart1 p1 p2 d = filter inRange [add2d d p1, add2d (neg2d d) p2]
  mkPart2 p1 p2 d = (takeWhile inRange $ iterate (add2d d) p1) ++ (takeWhile inRange $ iterate (add2d (neg2d d)) p2)

  solve f = antennasByFreq |> map (antinodes f) |> concat |> S.fromList |> S.size 

  part1 = solve mkPart1
  part2 = solve mkPart2
