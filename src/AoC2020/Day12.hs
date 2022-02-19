module AoC2020.Day12
( aoc202012
) where

import Utils (readInt)

data Instr = Shift (Int, Int) | Rotate Int | Forward Int

readInsts s = case (head s, readInt $ tail s) of
          ('N', n) -> Shift (  0,  n)
          ('S', n) -> Shift (  0, -n)
          ('E', n) -> Shift (  n,  0)
          ('W', n) -> Shift ( -n,  0)
          ('R', n) -> Rotate (360 - n)
          ('L', n) -> Rotate n
          ('F', n) -> Forward n

shift (x, y) (dx, dy) = (x  + dx, y + dy)
scale (x, y) n        = (x  *  n, y *  n)
manhattan (x, y)      = abs x + abs y
rotate  90 (x, y)     = (-y,  x)
rotate 180 (x, y)     = (-x, -y)
rotate 270 (x, y)     = ( y, -x)

rule1 (pos, dir) (Shift xxx) = (shift pos xxx, dir)
rule1 (pos, dir) (Rotate dg) = (pos, rotate dg dir)
rule1 (pos, dir) (Forward n) = (shift pos $ scale dir n, dir)

rule2 (pos, dir) (Shift xxx) = (pos, shift dir xxx)
rule2 (pos, dir) (Rotate dg) = (pos, rotate dg dir)
rule2 (pos, dir) (Forward n) = (shift pos $ scale dir n, dir)

solution f acc = manhattan . fst . foldl f acc . map readInsts . lines

aoc202012 input = (part1, part2) where
  part1 = solution rule1 ((0, 0), ( 1, 0)) input
  part2 = solution rule2 ((0, 0), (10, 1)) input
