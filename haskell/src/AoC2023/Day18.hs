module AoC2023.Day18
( aoc202318
) where

import Utils (readInt, (|>))
import Data.Char (isDigit)
import Data.List.Utils (split)
import Data.Foldable (foldl')
import qualified Data.Set as S
import Numeric (readHex)

addXY (x, y) (dx, dy) = (x + dx, y + dy)

main :: IO ()
main = do 
  input <- readFile "../inputs/2023/day18S.txt"
  input <- readFile "../inputs/2023/day18.txt"
  --putStrLn $ unlines $ map show $ instrs1
  --putStrLn $ unlines $ map show $ instrs2
  --putStrLn $ show $ part1
  --putStrLn $ show $ part2
  putStrLn $ show $ aoc202318 input

aoc202318 input = (part1, "MISSING") where
  inputs = input |> lines
  step (x, y) (dir, n) = case dir of
      'R' -> (x + n, y)
      'L' -> (x - n, y)
      'D' -> (x, y + n)
      'U' -> (x, y - n)

  tiles pos (dir, n) = [step pos (dir, k) | k <- [1..n]]

  solve instrs =
      let ext = instrs
                |> foldl' (\(coords, curPos) e -> (coords ++ tiles curPos e, step curPos e)) ([(0, 0)], (0, 0))
                |> fst
                |> S.fromList
      in length $ floodFill ext (S.singleton (1, 1))

  floodFill visited unvisited =
    if S.null unvisited
    then visited
    else let cur = S.elemAt 0 unvisited
             unvisited' = S.delete cur unvisited
             (visited', unvisited'') = if S.notMember cur visited
              then (S.insert cur visited, foldl' (\unv p ->S.insert (addXY cur p) unv) unvisited [(0, 1), (0, -1), (-1, 0), (1,0)])
              else (visited, unvisited')
          in floodFill visited' unvisited''

  instrs1 = inputs |> map (\s -> s |> words |> \[d, n, c] -> (head d, readInt n))
  part1 = solve instrs1

  mapDir '0' = 'R'
  mapDir '1' = 'D'
  mapDir '2' = 'L'
  mapDir '3' = 'U'

  instrs2 = inputs |> map (\s -> s |> words |> \[_, _, c] -> c |> drop 2 |> \h6 -> (mapDir $ head $ drop 5 h6, fst $ head $ readHex $ take 5 h6))
  part2 = solve instrs2
