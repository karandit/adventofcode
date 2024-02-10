module AoC2023.Day13
( aoc202313
) where

import Utils ((|>))
import Data.List (find, intersect, transpose)
import Data.List.Utils (split)
import Data.Foldable (foldl')

main :: IO ()
main = do 
  input <- readFile "../inputs/2023/day13S.txt"
  input <- readFile "../inputs/2023/day13.txt"
  putStrLn $ show $ aoc202313 input

aoc202313 input = (part1, part2) where
  pttrns = input |> lines |> split [""]
  pttrn1 = head pttrns
  pttrn2 = head $ tail pttrns
  vmirrors pttrn = let
        cs = length $ head pttrn
        match ln = [i | (i, (p1, p2)) <- zip [1..] $ zip ln (tail ln), p1 == p2] 
        by2 = foldl' (\acc ln -> intersect acc (match ln)) [1..cs] pttrn
        checkBy n = all (\ln -> all (\(a,b) -> a==b) $ zip (reverse $ take n ln) (drop n ln)) pttrn
      in filter checkBy by2

  hmirrors pttrn = vmirrors $ transpose $ map reverse pttrn
  part1 = sum [(sum $ vmirrors p) + 100 * (sum $ hmirrors p) | p <- pttrns ] 

  vmirrors2 pttrn = let
        cs = length $ head pttrn
        freqs = map (\ln -> map (\n -> length $ filter (\(a,b) -> a /=b)$ zip (reverse $ take n ln) (drop n ln))[1..length ln - 1] ) pttrn
      in maybe 0 fst $ find (\(idx, v) -> v == 1) $ zip [1..] $ map sum $ transpose freqs
  hmirrors2 pttrn = vmirrors2 $ transpose $ map reverse pttrn
  
  part2 = sum [(vmirrors2 p) + 100 * (hmirrors2 p) | p <- pttrns ] 
