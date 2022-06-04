module AoC2021.Day08
  ( aoc202108,
  )
where

import Data.List
import Data.List.Utils (split)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

segmentsToInt (ds, outputs) =
  let cf = ds |> filter (\s -> 2 == length s) |> head
      acf = ds |> filter (\s -> 3 == length s) |> head
      a = acf \\ cf
      bdcf = ds |> filter (\s -> 4 == length s) |> head
      bd = bdcf \\ cf
      adg = ds |> filter (\s -> 5 == length s) |> (\[x1, x2, x3] -> (x1 `intersect` x2) `intersect` x3)
      d = bd `intersect` adg
      b = bd \\ d
      g = (adg \\ d) \\ a
      agbf = ds |> filter (\s -> 6 == length s) |> (\[x1, x2, x3] -> (x1 `intersect` x2) `intersect` x3)
      f = ((agbf \\ a) \\ b) \\ g
      c = cf \\ f
      e = ((((("abcdefg" \\ a) \\ b) \\ c) \\ d) \\ f) \\ g
      wires = [(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')] |> M.fromList
      sevenSeg s = case s of
        "abcefg" -> 0
        "cf" -> 1
        "acdeg" -> 2
        "acdfg" -> 3
        "bcdf" -> 4
        "abdfg" -> 5
        "abdefg" -> 6
        "acf" -> 7
        "abcdefg" -> 8
        "abcdfg" -> 9
      segments s =
        s |> map (\ch -> M.lookup [ch] wires |> Maybe.fromMaybe ' ') |> sort |> sevenSeg

      [o1, o2, o3, o4] = outputs |> map segments
   in o1 * 1000 + o2 * 100 + o3 * 10 + o4

aoc202108 input = (part1, part2)
  where
    inputs = input |> lines |> map parseSegment
    parseSegment s = s |> split " | " |> map (split " ") |> \[a1, a2] -> (a1, a2)

    part1 =
      inputs
        |> map
          ( \(_, outputs) ->
              outputs
                |> map length
                |> filter (\l -> l == 2 || l == 4 || l == 3 || l == 7)
                |> length
          )
        |> sum

    part2 = inputs |> map segmentsToInt |> sum
