module AoC2023.Day03
( aoc202303
) where

import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.List (sortBy, groupBy)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Utils (readInt, perimeter8, (|>))

aoc202303 input = (part1, part2) where
  rows = lines input
  engine = M.fromList [ ((y, x), v) | (y, row) <- zip [0..] rows, (x, v) <- zip [0..] row ] 
  maxY = length rows - 1
  maxX = length (head rows) - 1 
  
  collectNrs y ((curN, curCoords), res) x = let
    c = engine M.! (y, x) 
    in if isDigit c
        then ((curN ++ [c], Set.insert (y,x) curCoords), res)
        else (("", Set.empty), if curN == "" then res else res ++ [(readInt curN, curCoords)]) 

  collectNrsByY acc y = let 
    ((curN, coords), nrs') = foldl' (collectNrs y)  (("", Set.empty), acc) [0..maxX]
    in if curN == "" then nrs' else nrs' ++ [(readInt curN, coords)]

  nrs = foldl' collectNrsByY [] [0..maxY]
  
  part1 = nrs |> filter (\(nr, coords) ->
                coords
                |> Set.foldl' (\acc yx -> Set.union acc (Set.fromList . perimeter8 $ yx)) Set.empty
                |> Set.filter (\yx -> M.lookup yx engine |> fmap (\c -> c /= '.' && not (isDigit c)) |> Maybe.fromMaybe False) 
                |> not . Set.null)
             |> map fst
             |> sum

  part2 = nrs |> map (\(nr, coords) ->
                (nr, coords
                |> Set.foldl' (\acc yx -> Set.union acc (Set.fromList . perimeter8 $ yx)) Set.empty
                |> Set.filter (\yx -> M.lookup yx engine |> fmap (\c -> c == '*') |> Maybe.fromMaybe False) 
                |> Set.toList
                ))
              |> filter (\(nr, coords) -> length coords == 1) 
              |> map (\(nr, coords) -> (nr, head coords))
              |> sortBy (\(nr1, gear1) (nr2, gear2) -> compare gear1 gear2)
              |> groupBy (\(nr1, gear1) (nr2, gear2) -> gear1 == gear2)
              |> filter (\gr -> length gr > 1)
              |> map (\gr -> map fst gr |> product) 
              |> sum
