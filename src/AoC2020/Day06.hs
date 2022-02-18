module AoC2020.Day06
( aoc202006
) where

import Data.List (union, intersect)
import Data.List.Utils (split)

foldQuestionsBy f = sum . map (length . foldl1 f) . split [""] . lines

aoc202006 input = (part1 input, part2 input) where
  part1 = foldQuestionsBy union
  part2 = foldQuestionsBy intersect
