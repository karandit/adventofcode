module AoC2015.Day01
( aoc201501a
, aoc201501b
) where

import Data.Maybe (fromMaybe)
import Data.List (findIndex)

direction '(' = 1
direction ')' = -1
direction  _  = 0

aoc201501a = sum . map direction
aoc201501b = fromMaybe (-500) . findIndex (< 0) . scanl (+) 0 . map direction
