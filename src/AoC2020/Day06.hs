module AoC2020.Day06
( aoc202006a
, aoc202006b
) where

import Data.List (union, intersect)
import Data.List.Utils (split)

foldQuestionsBy f = sum . map (length . foldl1 f) . split [""] . lines

aoc202006a = foldQuestionsBy union
aoc202006b = foldQuestionsBy intersect
