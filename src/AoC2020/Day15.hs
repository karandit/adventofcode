module AoC2020.Day15
( aoc202015a
, aoc202015b
) where

import qualified Data.Map as M
import Data.List
import Utils (insUpd)

guess stop round lastSpoken store = 
  if round == stop+1
  then lastSpoken
  else let nextSpoken = case M.lookup lastSpoken store of
                        Nothing -> 0 
                        Just lastBefore -> round - 1 - lastBefore
           nextStore = insUpd store lastSpoken (round - 1) 
       in guess stop (round + 1) nextSpoken nextStore


aoc202015a stop xs = guess stop (1 + length xs) (last xs) $ M.fromList $ zip xs [1..]
aoc202015b = aoc202015a
