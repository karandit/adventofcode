{-# LANGUAGE LambdaCase #-}
module AoC2022.Day12
  ( aoc202212,
  )
where

import Data.List (minimumBy, foldl')
import qualified Data.Map as M
import Data.Char (ord)
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

type XY = (Int, Int)

addXY (x, y) (dx, dy) = (x + dx, y + dy)
compareSnd (_, dista) (_, distb) = compare dista distb

aoc202212 input = (part1, part2) where
  charMap = M.fromList [((r,c), v) | (r, line) <- zip [0..] (lines input), (c, v) <- zip [0..] line]

  posS = charMap |> M.filter (== 'S') |> M.keys |> head
  posE = charMap |> M.filter (== 'E') |> M.keys |> head

  grid = charMap |> M.map (\case
                               'S' -> 'a'
                               'E' -> 'z'
                               c ->  c)
                 |> M.map (\c -> ord c - ord 'a')

  dijkstra :: (M.Map XY Int, Int -> Int -> Bool, M.Map XY Int, M.Map XY Int, Bool)
           -> (M.Map XY Int, Int -> Int -> Bool, M.Map XY Int, M.Map XY Int, Bool)
  dijkstra (heightmap, filterF, unvisited, visited, _) =
          let
          (xyCur, distCur) = unvisited |> M.toList |> minimumBy compareSnd
          heightCur = heightmap |> M.lookup xyCur |> Maybe.fromMaybe (-1)
          visited' = M.insert xyCur distCur visited
          unvisited' = M.delete xyCur unvisited
          unvisited'' = [(0, 1), (0, -1), (-1, 0), (1, 0)]
                   |> map (addXY xyCur)
                   |> filter (\xyNext -> M.notMember xyNext visited')
                   |> foldl' (\accUnvis xyNext -> heightmap |> M.lookup xyNext |> Maybe.maybe accUnvis
                                  (\heightNext -> if filterF heightNext heightCur
                                                then accUnvis
                                                else accUnvis |> M.alter
                                                  (\case
                                                       Nothing      -> Just (distCur + 1)
                                                       Just distOld -> Just (min distOld (distCur + 1))
                                                  ) xyNext
                                  )
                              ) unvisited'
          in (heightmap, filterF, unvisited'', visited', length unvisited'' == 0)


  findAllPaths startPos filterF = visited where
        (_, _, _, visited, _) = until
                (\(_, _, _, _, sol) -> sol)
                dijkstra
                (grid, filterF, M.singleton startPos 0, M.empty, False)

  visited1 = findAllPaths posS (\heightNext heightCur -> heightNext - heightCur > 1)
  visited2 = findAllPaths posE (\heightNext heightCur -> heightCur - heightNext > 1)

  part1 = visited1 |> M.lookup posE |> Maybe.fromMaybe (-1)
  part2 = charMap |> M.filter (== 'a') |> M.intersection visited2 |> M.elems |> minimum
