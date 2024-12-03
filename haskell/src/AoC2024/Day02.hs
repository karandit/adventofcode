module AoC2024.Day02
( aoc202402
) where

import Utils (readInt, (|>), freq)
import qualified Data.Map as M
import Data.Function (on)
import Data.List (sortBy, findIndex)

aoc202402 input = (part1, part2) where
  reports = input |> lines |> map (\s -> s |> words |> map readInt)

  isInRange x = x >= 1 && x <= 3

  isSafe1 report@(a:b:_) = case signum (a - b) of
      0 -> False
      sn -> all id $ zipWith (\e1 e2 -> isInRange (sn * (e1-e2)) ) report (tail report)      

  part1 = reports |> filter isSafe1 |> length

  dropAt nth xs = xs |> zip [0..] |> filter (\(i,_) -> i /= nth) |> map snd

  isSafe2 report =
    if isSafe1 report then True
    else let 
        diffs = zipWith (\e1 e2 -> e1 - e2) report (tail report)
        expected_signum = diffs |> map signum |> freq |> M.toList |> sortBy (compare `on` snd) |> reverse |> head |> fst
                   
        foundIdx = diffs |> findIndex (\diff -> not ((isInRange $ abs diff) && signum diff == expected_signum))
        in case foundIdx of
           Nothing -> True
           Just idx -> isSafe1 (dropAt idx report) || isSafe1 (dropAt (idx+1) report)

  part2 = reports |> filter isSafe2 |> length
