{-# LANGUAGE LambdaCase #-}
module AoC2022.Day24
( aoc202224
) where

import Utils ((|>))
import Data.List (foldl', minimumBy)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Strict
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Maybe as Maybe

type XY = (Int, Int)
type TXY = (Int, (Int, Int))
data Blizzard = R | L | D | U deriving (Show, Eq)
data Tile = Wall | Bs [Blizzard] deriving (Show, Eq)

-- showTile :: Tile -> Char
-- showTile Wall = '#'
-- showTile (Bs [U]) = '^'
-- showTile (Bs [D]) = 'v'
-- showTile (Bs [R]) = '>'
-- showTile (Bs [L]) = '<'
-- showTile (Bs bs) = length bs |> show |> head

addXY (x, y) (dx, dy) = (x + dx, y + dy)

compareTime ((m1, _), _) ((m2,_), _) = compare m1 m2

aoc202224 input = (part1, part1 + part2 + part3) where
  inputLines = input |> lines
  parseTile '#' = Wall
  parseTile '^' = Bs [U]
  parseTile 'v' = Bs [D]
  parseTile '>' = Bs [R]
  parseTile '<' = Bs [L]

  inputs = M.fromList [((x, y), parseTile c) | (y, row) <- zip [0..] inputLines, (x, c) <- zip [0..] row, c /= '.']
  maxX = inputLines |> head |> length |> pred
  maxY = inputLines |> length |> pred
  end = (maxX - 1, maxY)
  start = (1, 0)

  stepBliz (x, y) R = (if x + 1 == maxX then 1 else x + 1, y)
  stepBliz (x, y) L = (if x - 1 == 0 then maxX - 1 else x - 1, y)
  stepBliz (x, y) U = (x, if y - 1 == 0 then maxY - 1 else y - 1)
  stepBliz (x, y) D = (x, if y + 1 == maxY then 1 else y + 1)

  stepTile newTiles (xy, Wall) = newTiles |> M.insert xy Wall
  stepTile newTiles (xy, bs@(Bs items)) =
          let newItems = items |> map (\bli -> (stepBliz xy bli, bli))
          in newItems
             |> foldl' (\acc (newxy, bli) -> Data.Map.Strict.insertWith
                                  (\(Bs oldBlis) (Bs newBlis) -> Bs(oldBlis++newBlis)) newxy (Bs [bli]) acc) newTiles

  step tiles = foldl' stepTile M.empty (M.toList tiles)

  (stepCycle, cache, _) = until (\(min, _, tiles) -> min == 1000 ) --min /= 0 && tiles == inputs) -- TODO : this shouldn't be hardcoded as 1000
                         (\(min, cache, tiles) -> let newtiles = step tiles in (min + 1, M.insert (min+1) newtiles cache, newtiles))
                         (0, M.fromList [(0, inputs)], inputs)

  findShortest :: (XY, Map Int (Map XY Tile), Map TXY Int, Set TXY, Maybe Int)
               -> (XY, Map Int (Map XY Tile), Map TXY Int, Set TXY, Maybe Int)
  findShortest (target, cache, unvisited, visited, _) =
          let
          newmu = (mu + 1) -- `mod` stepCycle
          (ku@(mu, xyu), distu) = if null unvisited then (error "Unvisited is empty") else unvisited |> M.toList |> minimumBy compareTime
          visited' = S.insert ku visited
          unvisited' = M.delete ku unvisited
          newtiles = if M.member newmu cache then cache M.! newmu else (error "not found in cache")
          adjxys =  [(0, 1), (0, -1), (-1, 0), (1,0), (0, 0)] |> map (addXY xyu)
          newxys =  adjxys |> filter (\newxy@(newx, newy) -> S.notMember (newmu, newxy) visited &&
                                          (newxy == end || newxy == start || (newx > 0 && newx < maxX && newy > 0 && newy < maxY))
                                         && M.notMember newxy newtiles)

          unvisited'' = newxys
                    -- -  |> (\newxys -> if null newxys then error "Valami gaz van" else newxys)
                   |> foldl' (\acc xyadj -> acc |> M.alter
                                          (\case
                                               Nothing      -> Just (distu+1)
                                               Just distOld -> Just (min distOld (distu+1))
                                          ) (newmu, xyadj)
                              ) unvisited'
          in (target, cache, unvisited'', visited', if xyu == target then Just distu else Nothing)

  (_, _, _, _, Just part1) = until (\(_, _, _, _, sol) -> sol |> Maybe.isJust)
                                findShortest (end, cache, M.singleton (0, start) 0, S.empty, Nothing)
  (_, _, _, _, Just part2) = until (\(_, _, _, _, sol) -> sol |> Maybe.isJust)
                                findShortest (start, cache, M.singleton (part1, end) 0, S.empty, Nothing)
  (_, _, _, _, Just part3) = until (\(_, _, _, _, sol) -> sol |> Maybe.isJust)
                                findShortest (end, cache, M.singleton ((part1 + part2), start) 0, S.empty, Nothing)
