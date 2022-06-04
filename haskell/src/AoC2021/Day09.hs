module AoC2021.Day09
  ( aoc202109,
  )
where

import Data.List (foldl', sort)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import Utils (readInt, (|>))

aoc202109 input = (part1, part2)
  where
    inputs = input |> lines
    heights =
      zip [0 ..] inputs
        |> map (\(x, row) -> zip [0 ..] row |> map (\(y, c) -> ((x, y), readInt [c])))
        |> concat
    heightMap = heights |> M.fromList
    adjacents (x, y) = [(-1, 0), (0, -1), (0, 1), (1, 0)] |> map (\(dx, dy) -> (x + dx, y + dy))
    lowPoints =
      heights
        |> filter
          ( \(coord, v) ->
              coord
                |> adjacents
                |> all (\xy -> M.lookup xy heightMap |> Maybe.fromMaybe 10 |> \nv -> v < nv)
          )

    part1 = lowPoints |> map (\(_, v) -> v + 1) |> sum

    detectBasin visited coord =
      if M.member coord heightMap
        && (M.lookup coord heightMap |> Maybe.fromMaybe 10 |> \v -> v /= 9)
        && S.notMember coord visited
        then
          let visited' = S.insert coord visited
           in coord |> adjacents |> foldl' detectBasin visited'
        else visited

    part2 =
      lowPoints |> map (\(coord, _) -> coord |> detectBasin S.empty |> S.size)
        |> sort
        |> reverse
        |> take 3
        |> product
