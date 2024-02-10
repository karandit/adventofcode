module AoC2023.Day04
( aoc202304
) where

import Utils (readInt, (|>))
import Data.List (intersect)
import Data.List.Utils (split)
import Data.Foldable (foldl')
import qualified Data.Map as M

parseCard s = let
    [_, nrs] = split ": " s
    [winnings, yours] = split " | " nrs |> map (map readInt . words)
    in (winnings, yours)

wins (ws, ys) = intersect ws ys |> length
 
aoc202304 input = (part1, part2) where
  cards = input |> lines |> map parseCard

  part1 =  sum [2 ^ (x - 1) | x <- map wins cards, x > 0] 

  initState = M.fromList [(i, 1) | i <- [1..(length cards)]]
  part2 = cards
          |> zip [1..]
          |> foldl' (\state (i, card) -> let
                extra = state M.! i
                copies = [i + 1 .. i + wins card]
                in foldl' (\st cp -> M.adjust (+extra) cp st) state copies
            ) initState 
          |> M.elems
          |> sum
