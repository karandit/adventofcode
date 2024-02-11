module AoC2023.Day19
( aoc202319
) where

import Utils (readInt, (|>))
import Data.List.Utils (split, replace)
import qualified Data.Map as M

data Rule = Proc String | Cond (Char, Char, Int, String)  deriving (Show)

parsePart s = s |> replace "{" "" |> replace "}" "" |> split ","
    |> map (split "=")
    |> map (\[k, v] -> (head k, readInt v)) |> M.fromList

parseWorkflow s = s |> split "{"
    |> (\[name, r] -> (name, r |> replace "}" "" |> split "," |> map parseRule))

parseRule s = if elem ':' s
    then s |> split ":" |> \[r:c:val, dst] -> Cond (r, c, (readInt val), dst)
    else Proc s
                
main :: IO ()
main = do 
  input <- readFile "../inputs/2023/day19S.txt"
  input <- readFile "../inputs/2023/day19.txt"
  let
  -- putStrLn $ "------------"
  -- putStrLn $ unlines $ map show $ ws'
  -- putStrLn $ ""
  -- putStrLn $ unlines $ map show $ parts
  -- putStrLn $ "------------"
  -- putStrLn $ show $ procPart p1
  -- putStrLn $ show $ part1
  putStrLn $ show $ aoc202319 input

aoc202319 input = (part1, "MISSING") where
  [ws, ps] = input |> lines |> split [""]
  ws' = ws |> map parseWorkflow
  wfs = ws' |> M.fromList
  parts = ps |> map parsePart
  
  p1 = head $ tail parts 

  procRules part [Proc dst] = case dst of
              "A" -> True
              "R" -> False
              _   -> procRules part (wfs M.! dst)
  procRules part (Cond (fRat, fOp, val, dst):rs) = let
        rat = (part M.! fRat) 
        isMatching = case fOp of
                  '<' -> rat < val
                  '>' -> rat > val
      in if isMatching
         then case dst of
              "A" -> True
              "R" -> False
              _   -> procRules part (wfs M.! dst)
         else procRules part rs
      
  procPart part = procRules part (wfs M.! "in")
  accepteds = filter procPart parts
  part1 = accepteds |> map (sum . M.elems) |> sum
