module AoC15.Day01
( aoc201501a
, aoc201501b
) where

direction c = case c of
  '(' -> 1
  ')' -> -1
  otherwise  -> 0

aoc201501a =
    sum . map direction

aoc201501b =
    length . takeWhile (> -1) . scanl (+) 0 . map direction
