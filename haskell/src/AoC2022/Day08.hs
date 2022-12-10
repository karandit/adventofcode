module AoC2022.Day08
  ( aoc202208,
  )
where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

aoc202208 input = (part1, part2)
  where
    addXY (x, y) (dx, dy) = (x + dx, y + dy)
    directions = [(0, -1), (0, 1), (-1, 0), (1, 0)]

    trees =
      input
        |> lines
        |> zip [0 ..] |> map ( \(r, row) ->
              row |> zip [0 ..] |> map
                  ( \(c, v) -> ((r, c), readInt [v]))
          )
        |> concat
        |> M.fromList

    walkFromTreeInDir k0 v0 f acc0 dxy =
      until
        (\(k, acc, stop) -> stop)
        ( \(k, acc, stop) ->
            let k' = addXY k dxy
                (acc', stop') = trees |> M.lookup k' |> fmap (\v -> (f v v0 acc, v >= v0)) |> Maybe.fromMaybe (acc, True)
             in (k', acc', stop')
        )
        (k0, acc0, False)
        |> (\(_, result, _) -> result)

    isVisible k0 v0 =
      let isVisibleInDir dir = walkFromTreeInDir k0 v0 (\v v0 acc -> v < v0) True dir
       in directions |> map isVisibleInDir |> any id

    scenicScore k0 v0 =
      let scoreInDir dir = walkFromTreeInDir k0 v0 (\v v0 acc -> acc + 1) 0 dir
       in directions |> map scoreInDir |> product

    part1 = trees |> M.filterWithKey isVisible |> M.size
    part2 = trees |> M.mapWithKey scenicScore |> M.elems |> maximum
