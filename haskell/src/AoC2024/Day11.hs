module AoC2024.Day11
( aoc202411
) where

import Utils (readInt, (|>))
import Data.List (foldl')
import qualified Data.Map as M

aoc202411 input = (part1, part2) where
  inputs = input |> lines |> head |> words

  leftTrim [] = []
  leftTrim [x] = [x]
  leftTrim ('0':x) = leftTrim x
  leftTrim n = n

  change :: String -> [String]
  change stone
    | stone == "0"              = ["1"]
    | length stone `mod` 2 == 0 = let n = length stone`div` 2 in [take n stone] ++ [leftTrim $ drop n stone]
    | otherwise                 = [show $ readInt stone * 2024]

  blink :: Int -> (M.Map (String, Int) Int, Int) -> String -> (M.Map (String, Int) Int, Int)
  blink 0 (memo, acc) stone = (memo, acc + 1)
  blink n (memo, acc) stone = case M.lookup (stone, n) memo of
    Nothing ->
      let (memo', v) = foldl' (blink (n-1)) (memo, 0) (change stone)
      in (M.insert (stone, n) v memo', acc + v)
    Just v -> (memo, acc + v)

  solve memo n = foldl' (blink n) (memo, 0) inputs

  (memo1, part1) = solve M.empty 25
  (_, part2) = solve memo1 75
