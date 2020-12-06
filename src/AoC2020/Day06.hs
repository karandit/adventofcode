module AoC2020.Day06
( aoc202006a
, aoc202006b
) where

import Data.List
import qualified Data.Map as M
import Utils

fold1 (store,count) ""  = (M.empty, count + M.size store)
fold1 (store,count) row = (M.union store $ freq row, count) 

fold2 (store,count) ""  = ([], count + (length $ foldl (intersect) (head store) (tail store)))
fold2 (store,count) row = (row:store , count) 

foldBlocks folder initStore input = 
  let res = foldl folder (initStore, 0) $ lines input 
  in snd $ folder res ""

aoc202006a = foldBlocks fold1 M.empty
aoc202006b = foldBlocks fold2 []
