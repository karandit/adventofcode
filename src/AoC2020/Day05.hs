module AoC2020.Day05
( aoc202005a
, aoc202005b
, seatId
) where

import Data.List

seatId l =
  let
    binToDec upper = sum . map (2^) . findIndices (==upper) . reverse
    rowId = binToDec 'B' $ take 7 l
    colId = binToDec 'R' $ drop 7 l
  in  rowId * 8 + colId

aoc202005a input =
  maximum $ map seatId $ lines input

aoc202005b input =
  let seatIds = sort $ map seatId $ lines input
      firstSeatId = head seatIds
  in maybe 0 fst $ find (\(i,v) -> i /= v) $ zip [firstSeatId..] seatIds
