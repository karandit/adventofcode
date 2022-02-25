module AoC2021.Day02
  ( aoc202102,
  )
where

import Utils (readInt, (|>))

parseMove ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : x) = (readInt x, 0)
parseMove ('d' : 'o' : 'w' : 'n' : x) = (0, readInt x)
parseMove ('u' : 'p' : x) = (0, - (readInt x))

aoc202102 input = (part1, part2)
  where
    moves = input |> lines |> map parseMove

    part1 =
      moves
        |> foldl1 (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2))
        |> \(x, y) -> x * y
    part2 =
      moves
        |> foldl (\(x1, y1, aim) (x2, y2) -> (x1 + x2, y1 + (x2 * (aim + y2)), aim + y2)) (0, 0, 0)
        |> \(x, y, _) -> x * y
