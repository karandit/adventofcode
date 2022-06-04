module AoC2021.Day03
  ( aoc202103,
  )
where

import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Data.Map as M
import Utils (bin2dec, freq, (|>))

calcRate posFunc inputs =
  inputs |> concat |> freq |> M.toList
    |> map (\((idx, bit), fr) -> (idx, (bit, fr)))
    |> sortBy (compare `on` fst)
    |> groupBy (\(x1, _) (x2, _) -> x1 == x2)
    |> map (\freqs -> freqs |> map (\(idx, (bit, fr)) -> (bit, fr)) |> sortBy (compare `on` snd) |> posFunc |> fst)
    |> bin2dec

supportRating idx crit [input] = input |> bin2dec
supportRating idx crit inputs =
  let bitCriteria =
        inputs
          |> map (\input -> input !! idx)
          |> freq
          |> M.toList
          |> sortBy (compare `on` snd)
          |> crit
          |> fst
      keptInputs =
        inputs
          |> filter (\input -> bitCriteria == input !! idx)
   in supportRating (idx + 1) crit keptInputs

aoc202103 input = (part1, part2)
  where
    inputs = input |> lines
    inputBits = inputs |> map (zip [0 ..])

    gamma = calcRate last inputBits
    epsilon = calcRate head inputBits
    part1 = gamma * epsilon

    co2ScRat = supportRating 0 last inputs
    o2GenRat = supportRating 0 head inputs
    part2 = co2ScRat * o2GenRat
