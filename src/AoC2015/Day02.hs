module AoC2015.Day02
( aoc201502a
, aoc201502b
, parseInput201502)
where

import Utils

parseInput201502 =
  let readNums = map readInt . words . replace 'x' ' '
  in  map readNums . lines

aoc201502a [a,b,c] =
  let surfaces = [a*b, a*c, b*c]
  in 2 * (sum surfaces) + minimum surfaces

aoc201502b ns@[a,b,c] =
  let perimeters = [a+b, a+c, b+c]
  in 2 * (minimum perimeters) + product ns
