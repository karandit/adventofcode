module AoC2023.Day10
( aoc202310
) where

import Utils (readInt, (|>))
import Data.Char (isDigit)
import Data.List.Utils (split)
import Data.Foldable (foldl')
import qualified Data.Map as M

main :: IO ()
main = do 
  input <- readFile "../inputs/2023/day10S.txt"
  input <- readFile "../inputs/2023/day10XS.txt"
  input <- readFile "../inputs/2023/day10.txt"
  putStrLn $ show $ aoc202310 input

aoc202310 input = (part1, "MISSING") where
  rows = lines input
  pipes' = M.fromList [ ((y, x), v) | (y, row) <- zip [0..] rows, (x, v) <- zip [0..] row ] 
  foundS = M.filter ((==) 'S') pipes'
  coordS = foundS |> M.keys |> head
  pipes = M.insert coordS 'F' pipes'
  dirS = 3
--  0    
--3   1
--  2
  nextPipe 0 '|' = (0, (-1,  0))
  nextPipe 2 '|' = (2, ( 1,  0))

  nextPipe 1 '-' = (1, ( 0,  1)) 
  nextPipe 3 '-' = (3, ( 0, -1))

  nextPipe 2 'L' = (1, ( 0,  1))
  nextPipe 3 'L' = (0, (-1,  0)) 

  nextPipe 2 'J' = (3, ( 0, -1))
  nextPipe 1 'J' = (0, (-1,  0))

  nextPipe 1 '7' = (2, ( 1,  0))
  nextPipe 0 '7' = (3, ( 0, -1))

  nextPipe 3 'F' = (2, ( 1,  0))
  nextPipe 0 'F' = (1, ( 0,  1))

  (_, coordsP1) = until
      (\((d, yx), acc) -> length acc /= 0 && yx == coordS)
      (\((d, yx@(y, x)), acc) -> let 
          (newd, (dy, dx)) = nextPipe d (pipes M.! yx)
          in ((newd, (y+dy, x+dx)), yx:acc))
      ((dirS, coordS) , [])
  part1 = (length coordsP1) `div` 2
