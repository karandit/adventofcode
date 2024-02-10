module AoC2023.Day09
( aoc202309
) where

import Utils (readInt, (|>))
import Data.Foldable (foldl')

aoc202309 input = (part1, part2) where
  inputs = input |> lines |> map (map readInt . words)
  diffs ns = [b - a | (a, b) <- zip ns (tail ns)]

  calc1 ls = let 
      (_, es) = until
          (\(xs, _) -> all ((==) 0) xs)
          (\(xs, acc)  -> (diffs xs, (last xs) + acc)) 
          (ls, 0)
      in es
  calc2 ls = let 
       (_, es) = until
           (\(xs, _) -> all ((==) 0) xs)
           (\(xs, acc)  -> (diffs xs, (head xs)  : acc)) 
           (ls, [])
       in foldl' (\acc e -> e - acc) 0 es

  part1 = sum $ map calc1 inputs
  part2 = sum $ map calc2 inputs
