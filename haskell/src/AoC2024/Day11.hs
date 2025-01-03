module AoC2024.Day11
( aoc202411
) where

import Utils (readInt)
import Data.List (foldl')
import qualified Data.Map as M

aoc202411 input = (part1, part2) where
  inputs = [readInt w | w <- words $ head $ lines $ input]

  change :: Int -> [Int]
  change 0 = [1]
  change stone
    | (w, 0) <- length (show stone) `quotRem` 2
    , (l, r) <- stone `quotRem` (10 ^ w) = [l, r]
  change stone = [stone * 2024]

  blink :: Int -> (M.Map (Int, Int) Int, Int) -> Int -> (M.Map (Int, Int) Int, Int)
  blink 0 (memo, acc) _     = (memo, acc + 1)
  blink n (memo, acc) stone = case M.lookup (stone, n) memo of
    Nothing ->
      let (memo', v) = foldl' (blink (n-1)) (memo, 0) (change stone)
      in (M.insert (stone, n) v memo', acc + v)
    Just v -> (memo, acc + v)

  solve memo n = foldl' (blink n) (memo, 0) inputs

  (memo1, part1) = solve M.empty 25
  (_, part2) = solve memo1 75
