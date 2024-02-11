module AoC2023.Day17
( aoc202317
) where

import Utils (readInt, (|>))
import qualified Data.Map as M

main :: IO ()
main = do 
  input <- readFile "../inputs/2023/day17S.txt"
  --input <- readFile "../inputs/2023/day17.txt"
  let 
    inputs = input |> lines
    grid = M.fromList [((y, x), readInt [v]) | (y, row) <- zip [0..] inputs, (x, v) <- zip [0..] row] 
  putStrLn $ unlines $ map show $ inputs
  --putStrLn $ unlines $ map show $ M.toList grid

aoc202317 input = ("MISSING", "MISSING")
