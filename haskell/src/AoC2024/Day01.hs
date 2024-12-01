module AoC2024.Day01
( aoc202401
) where

import Utils (readInt, freq)
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

aoc202401 input = (part1, part2) where
  (fsts, snds) = unzip [(a, b) | [a, b] <- map (map readInt . words) $ lines input]
  part1 = sum [abs (a - b) | (a, b) <- zip (sort fsts) (sort snds)]
  freqs = freq snds
  part2 = sum [Maybe.maybe 0 (*a) $ M.lookup a freqs | a <- fsts]
