module AoC2021.Day04
  ( aoc202104,
  )
where

import Data.List
import qualified Data.List.Utils as LL
import qualified Data.Map as M
import qualified Data.String.Utils as SS
import Utils (readInt, (|>))

type Coord = (Int, Int)

type Board = M.Map Coord (Int, Bool)

parseBoard :: [String] -> Board
parseBoard rows =
  rows
    |> zip [0 ..]
    |> map
      ( \(rowIdx, row) ->
          row
            |> SS.lstrip
            |> SS.replace "  " " "
            |> LL.split " "
            |> zip [0 ..]
            |> map (\(colIdx, c) -> ((rowIdx, colIdx), (readInt c, False)))
      )
    |> concat
    |> M.fromList

findBingo :: [Board] -> Int -> [Board] -> [Int] -> ([Board], [Int])
findBingo [] nr newBoards newBingos = (newBoards, newBingos)
findBingo (board : boards) nr newBoards newBingos =
  let foundCoord = board |> M.toList |> find (\(_, (v, _)) -> nr == v)
   in case foundCoord of
        Nothing -> findBingo boards nr (board : newBoards) newBingos
        Just ((row, col), _) ->
          let board' = M.adjust (\(v, _) -> (v, True)) (row, col) board
              isBingo = checkBingo (row, col) board'
           in if isBingo
                then findBingo boards nr newBoards ((bingoScore board' nr) : newBingos)
                else findBingo boards nr (board' : newBoards) newBingos

checkBingo :: Coord -> Board -> Bool
checkBingo (row, col) board =
  let isFiveMarked coords = coords |> all (\coord -> M.lookup coord board |> maybe False snd)
   in isFiveMarked (zip (cycle [row]) [0 .. 4])
        || isFiveMarked (zip [0 .. 4] (cycle [col]))

bingoScore :: Board -> Int -> Int
bingoScore board nr =
  let unmarkedsSum = board |> M.toList |> filter (\(_, (_, marked)) -> not marked) |> map (\(_, (v, _)) -> v) |> sum
   in nr * unmarkedsSum

playBingoAll [] _ bingos = bingos
playBingoAll _ [] bingos = bingos
playBingoAll boards (nr : nrs) bingos =
  let (newBoards, newBingos) = findBingo boards nr [] []
   in playBingoAll newBoards nrs (newBingos ++ bingos)

aoc202104 input = (part1, part2)
  where
    inputs = input |> lines |> LL.split [""]
    nrs = inputs |> head |> head |> LL.split "," |> map readInt
    boards = inputs |> tail |> map parseBoard
    bingos = playBingoAll boards nrs []

    part1 = last bingos
    part2 = head bingos
