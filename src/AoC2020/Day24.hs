module AoC2020.Day24
  ( aoc202024,
  )
where

import Data.List (group, sort)
import qualified Data.Set as S
import Utils ((|>))

data Hex = East | West | SouthEast | SouthWest | NorthEast | NorthWest

shiftPos :: (Int, Int) -> Hex -> (Int, Int)
shiftPos (x, y) East = (x + 1, y)
shiftPos (x, y) West = (x - 1, y)
shiftPos (x, y) SouthEast = (x + abs (y `mod` 2), y - 1)
shiftPos (x, y) SouthWest = (x + abs (y `mod` 2) - 1, y - 1)
shiftPos (x, y) NorthEast = (x + abs (y `mod` 2), y + 1)
shiftPos (x, y) NorthWest = (x + abs (y `mod` 2) - 1, y + 1)

shiftAndParseTile pos cs dir = parseTile (shiftPos pos dir) cs

parseTile pos [] = pos
parseTile pos ('e' : cs) = shiftAndParseTile pos cs East
parseTile pos ('w' : cs) = shiftAndParseTile pos cs West
parseTile pos ('s' : 'e' : cs) = shiftAndParseTile pos cs SouthEast
parseTile pos ('s' : 'w' : cs) = shiftAndParseTile pos cs SouthWest
parseTile pos ('n' : 'e' : cs) = shiftAndParseTile pos cs NorthEast
parseTile pos ('n' : 'w' : cs) = shiftAndParseTile pos cs NorthWest

nextFloor 0 floor = floor
nextFloor cnt floor = nextFloor (cnt - 1) (genFloor floor)

adjacents pos = map (shiftPos pos) [East, West, SouthEast, SouthWest, NorthEast, NorthWest]

genFloor :: S.Set (Int, Int) -> S.Set (Int, Int)
genFloor blackTiles =
  let isSurvivor pos =
        let blackNeighs = pos |> adjacents |> filter (\adj -> S.member adj blackTiles) |> length
         in blackNeighs == 1 || blackNeighs == 2
      survivors = blackTiles |> S.filter isSurvivor
      newborns =
        blackTiles |> S.toList
          |> map adjacents
          |> concat
          |> sort
          |> group
          |> filter (\gr -> length gr == 2)
          |> map (\gr -> head gr)
          |> filter (\pos -> S.member pos blackTiles |> not)
          |> S.fromList
   in S.union newborns survivors

readBlackTiles input = input |> lines |> map (parseTile (0, 0)) |> sort |> group |> filter (\gr -> length gr `mod` 2 /= 0) |> map (\gr -> head gr)

aoc202024 input = (part1, part2)
  where
    part1 = input |> readBlackTiles |> length
    part2 = input |> readBlackTiles |> S.fromList |> nextFloor 100 |> S.size
