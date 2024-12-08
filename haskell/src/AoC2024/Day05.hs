module AoC2024.Day05
( aoc202405
) where

import Utils (readInt, (|>))
import Data.List.Utils (split)
import Data.List (sortBy, tails)
import Data.Foldable (foldl')
import qualified Data.Map as M
import qualified Data.Set as S

aoc202405 input = (part1, part2) where
  [raw_rules, raw_updates] = input |> lines |> split [""]
  rules = [(a, b) | r <- raw_rules, let [a, b] = r |> split "|" |> map readInt]
  updates = [u |> split "," |> map readInt | u <- raw_updates]
  rulesMap = foldl' (\acc (k, v) -> M.insertWith S.union k (S.singleton v) acc) M.empty rules

  followers n = M.findWithDefault S.empty n rulesMap
  isOrdered u = and [(S.fromList nRest) `S.isSubsetOf` (followers nFirst) | nFirst : nRest <- tails u]

  solve p f = sum [u' !! (length u' `div` 2)| u <- updates, p u , let u' = f u]

  part1 = solve isOrdered id
  part2 = solve (not . isOrdered) (sortBy (\a b -> if b `S.member` (followers a) then LT else GT))
