module AoC2022.Day09
  ( aoc202209,
  )
where

import Data.List.Utils (split)
import qualified Data.Set as S
import Utils (readInt, (|>))

aoc202209 input = (part1, part2)
  where
    moves =
      input
        |> lines
        |> map (\x -> x |> split " " |> (\[a, b] -> (head a, readInt b)))

    add (x, y) (dx, dy) = (x + dx, y + dy)
    isFar (x1, y1) (x2, y2) = abs (x1 - x2) > 1 || abs (y1 - y2) > 1

    pullKnot prevKnot@(px, py) knot@(x, y) =
      let moveWith =
            if isFar prevKnot knot
              then (signum (px - x), signum (py - y))
              else (0, 0)
       in add knot moveWith

    pullRope (rope, visited) dir =
      let (x0, y0) = head rope
          newhead = case dir of
            'R' -> (x0 + 1, y0)
            'L' -> (x0 - 1, y0)
            'U' -> (x0, y0 - 1)
            'D' -> (x0, y0 + 1)
          newrope = scanl pullKnot newhead (tail rope)
          newtail = last newrope
       in (newrope, S.insert newtail visited)

    solve n =
      let rope = repeat (0, 0) |> take n
       in moves
            |> foldl (\acc (dir, d) -> repeat dir |> take d |> foldl pullRope acc) (rope, S.singleton (0, 0))
            |> snd
            |> length
    part1 = solve 2
    part2 = solve 10
