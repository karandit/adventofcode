module AoC2020.Day05
  ( aoc202005,
  )
where

import Data.List

-- | Calculates the seat id
--
-- >>> seatId "FBFBBFFRLR"
-- 357
seatId l =
  let binToDec upper = sum . map (2 ^) . findIndices (== upper) . reverse
      rowId = binToDec 'B' $ take 7 l
      colId = binToDec 'R' $ drop 7 l
   in rowId * 8 + colId

aoc202005 input = (part1, part2)
  where
    seatIds = map seatId $ lines input
    part1 = maximum $ seatIds

    sortedSeatIds = sort $ seatIds
    firstSeatId = head sortedSeatIds
    part2 = maybe 0 fst $ find (\(i, v) -> i /= v) $ zip [firstSeatId ..] sortedSeatIds
