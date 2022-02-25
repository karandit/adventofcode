module AoC2020.Day15
  ( aoc202015,
  )
where

import Data.List
import Data.List.Utils (split)
import qualified Data.Map as M
import Utils (insUpd, readInt, (|>))

guess stop round lastSpoken store =
  if round == stop + 1
    then lastSpoken
    else
      let nextSpoken = case M.lookup lastSpoken store of
            Nothing -> 0
            Just lastBefore -> round - 1 - lastBefore
          nextStore = insUpd store lastSpoken (round - 1)
       in guess stop (round + 1) nextSpoken nextStore

-- | Recently spoken number after stop rounds
--
-- >>> spokenNr 2020 [1,3,2]
-- 1
-- >>> spokenNr 2020 [2,1,3]
-- 10
-- >>> spokenNr 2020 [1,2,3]
-- 27
-- >>> spokenNr 2020 [2,3,1]
-- 78
-- >>> spokenNr 2020 [3,2,1]
-- 438
-- >>> spokenNr 2020 [3,1,2]
-- 1836
spokenNr stop xs = guess stop (1 + length xs) (last xs) $ M.fromList $ zip xs [1 ..]

aoc202015 input = (spokenNr 2020 nrs, "MISSING")
  where
    nrs = input |> lines |> head |> split "," |> map readInt
