module AoC2023.Day10
( aoc202310
) where

import Utils (areaShoelace, (|>))
import Data.List (tails)
import qualified Data.Map as M

aoc202310 input = (part1,part2) where
  rows = lines input
  pipes' = M.fromList [ ((y, x), v) | (y, row) <- zip [0..] rows, (x, v) <- zip [0..] row ] 
  coordS = M.filter ((==) 'S') pipes' |> M.keys |> head
  pipes = M.insert coordS 'F' pipes'
  dirS = 3 -- north = 0, east = 1, sounth = 2, west = 3

  nextPipe :: Int -> Char -> (Int, (Int, Int))
  nextPipe dir c = case (dir, c) of
    (0, '|') -> (0, (-1,  0))
    (2, '|') -> (2, ( 1,  0))
    (1, '-') -> (1, ( 0,  1))
    (3, '-') -> (3, ( 0, -1))
    (2, 'L') -> (1, ( 0,  1))
    (3, 'L') -> (0, (-1,  0))
    (2, 'J') -> (3, ( 0, -1))
    (1, 'J') -> (0, (-1,  0))
    (1, '7') -> (2, ( 1,  0))
    (0, '7') -> (3, ( 0, -1))
    (3, 'F') -> (2, ( 1,  0))
    (0, 'F') -> (1, ( 0,  1))

  (_, coords) = until
      (\((d, yx), acc) -> length acc /= 0 && yx == coordS)
      (\((d, yx@(y, x)), acc) -> let 
          (newd, (dy, dx)) = nextPipe d (pipes M.! yx)
          in ((newd, (y+dy, x+dx)), yx:acc))
      ((dirS, coordS) , [])
  part1 = (length coords) `div` 2

  coordsCycle = coordS:coords
  area = areaShoelace coordsCycle
  boundary = length coords
  part2 = area + 1 - boundary `quot` 2 -- based on Pick's theorem
