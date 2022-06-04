module AoC2021.Day14
  ( aoc202114,
  )
where

import qualified Data.List.Utils as LL
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Utils (applyN, readInt, (|>))

aoc202114 input = (part1, part2)
  where
    inputs = input |> lines
    polymerParts = inputs |> head
    rules = inputs |> drop 2 |> map (\s -> s |> LL.split " -> " |> (\[[k1, k2], [v]] -> ([k1, k2], ([k1, v], [v, k2])))) |> M.fromList
    firstLet = head polymerParts
    lastLet = last polymerParts
    polymer0 = zip polymerParts (tail polymerParts) |> map (\(k1, k2) -> ([k1, k2], 1)) |> M.fromList

    stepPolymer polymer =
      polymer
        |> M.foldrWithKey
          ( \k count acc ->
              M.lookup k rules
                |> Maybe.maybe
                  acc
                  ( \(e1, e2) ->
                      acc
                        |> M.insertWith (+) e1 count
                        |> M.insertWith (+) e2 count
                  )
          )
          M.empty
    countLetters firstL lastL polymer =
      polymer
        |> M.foldrWithKey (\[k1, k2] v acc -> acc |> M.insertWith (+) k1 v |> M.insertWith (+) k2 v) M.empty
        |> M.adjust succ firstL
        |> M.adjust succ lastL
        |> M.map (\x -> x `div` 2)
    diffMostLeast counts = (maximum counts) - (minimum counts)
    part1 = polymer0 |> applyN 10 stepPolymer |> countLetters firstLet lastLet |> diffMostLeast
    part2 = polymer0 |> applyN 40 stepPolymer |> countLetters firstLet lastLet |> diffMostLeast
