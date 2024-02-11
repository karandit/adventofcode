module AoC2023.Day16
( aoc202316
) where

import Utils ((|>))
import qualified Data.Map as M
import qualified Data.Set as S

aoc202316 input = (part1, part2) where
  inputs = input |> lines
  grid = M.fromList [((y, x), v) | (y, row) <- zip [0..] inputs, (x, v) <- zip [0..] row]

  nextPos (y, x) '>' = (y, x + 1)
  nextPos (y, x) '<' = (y, x - 1)
  nextPos (y, x) 'v' = (y + 1, x)
  nextPos (y, x) '^' = (y - 1, x)

  nextDir d  '.' = [d]
  nextDir '>' '|' = ['^', 'v']
  nextDir '>' '-' = ['>']
  nextDir '>' '\\' = ['v']
  nextDir '>' '/' = ['^']

  nextDir '<' '|' = ['^', 'v']
  nextDir '<' '-' = ['<']
  nextDir '<' '\\' = ['^']
  nextDir '<' '/' = ['v']

  nextDir 'v' '|' = ['v']
  nextDir 'v' '-' = ['<', '>']
  nextDir 'v' '\\' = ['>']
  nextDir 'v' '/' = ['<']

  nextDir '^' '|' = ['^']
  nextDir '^' '-' = ['<', '>']
  nextDir '^' '\\' = ['<']
  nextDir '^' '/' = ['>']

  step (pos, d) = let
       nPos = nextPos pos d
       in if M.member nPos grid
       then let c = grid M.! nPos
                nDirs = nextDir d c
            in map (\nd -> (nPos, nd)) nDirs
       else []

  energized startPos = let
      (_, seenPart1, rounds) = until
          (\(beams, seen, rounds) -> null beams)
          (\(beams, seen, rounds)  -> (filter (\beam -> not $ S.member beam seen) $ concatMap step beams, beams |> S.fromList |> S.union seen, rounds + 1))
          ([startPos], S.empty, 0)
      in seenPart1 |> S.map fst |> length |> pred

  part1 = energized ((0, -1), '>')
  maxY = length inputs
  maxX = length $ head inputs
  part2 = let
      maxLeft   = maximum [energized ((y, -1),   '>')| y <- [0..maxY]]
      maxRight  = maximum [energized ((y, maxX), '<')| y <- [0..maxY]]
      maxTop    = maximum [energized ((-1, x),   'v')| x <- [0..maxX]]
      maxBottom = maximum [energized ((maxY, x), '^')| x <- [0..maxX]]
      in maximum [maxLeft, maxRight, maxTop, maxBottom]
