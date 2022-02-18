module AoC2015.Day01
( aoc201501a
, aoc201501b
, aoc201501
) where

import Data.Maybe (fromMaybe)
import Data.List (findIndex)

direction '(' = 1
direction ')' = -1
direction  _  = 0

aoc201501a = sum . map direction
aoc201501b = fromMaybe (-500) . findIndex (< 0) . scanl (+) 0 . map direction

aoc201501 :: String -> (Int, Int)
aoc201501 input = (aoc201501a input, aoc201501b input)
