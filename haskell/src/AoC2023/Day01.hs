module AoC2023.Day01
( aoc202301
) where

import Utils (readInt)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.List.Utils (replace)

aoc202301 input = (part1, part2) where
  solve f = sum [readInt $ [head n] ++ [last n] | s <- lines input, let n = filter isDigit $ f s]

  part1 = solve id
  
  insertDigit s to@(h:_:r) = replace (h:r) to s
  insertDigits s = foldl' insertDigit s 
        ["o1ne", "t2wo", "t3hree", "f4our", "f5ive", "s6ix", "s7even", "e8ight", "n9ine"]
  part2 = solve insertDigits
