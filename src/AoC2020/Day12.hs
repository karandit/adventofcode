module AoC2020.Day12
( aoc202012a
, aoc202012b
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

part1 (pos, dir) (Shift xxx) = (shift pos xxx, dir)
part1 (pos, dir) (Rotate dg) = (pos, rotate dg dir)
part1 (pos, dir) (Forward n) = (shift pos $ scale dir n, dir)

part2 (pos, dir) (Shift xxx) = (pos, shift dir xxx)
part2 (pos, dir) (Rotate dg) = (pos, rotate dg dir)
part2 (pos, dir) (Forward n) = (shift pos $ scale dir n, dir)

solution f acc = manhattan . fst . foldl f acc . map readInsts . lines

aoc202012a = solution part1 ((0, 0), ( 1, 0))
aoc202012b = solution part2 ((0, 0), (10, 1))
