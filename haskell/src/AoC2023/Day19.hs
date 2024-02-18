module AoC2023.Day19
( aoc202319
) where

import Utils (readInt, (|>))
import Data.List.Utils (split, replace)
import qualified Data.Map as M

data Rule = Proc String | Cond (Char, Char, Int, String) deriving (Show)
type Part = M.Map Char Int -- for part 1
type Ranges = M.Map Char (Int, Int) -- for part 2

parsePart s = s |> replace "{" "" |> replace "}" "" |> split ","
    |> map (split "=")
    |> map (\[k, v] -> (head k, readInt v))
    |> M.fromList

parseWorkflow s = s |> split "{"
    |> (\[name, r] -> (name, r |> replace "}" "" |> split "," |> map parseRule))

parseRule s = if elem ':' s
    then s |> split ":" |> \[r:c:val, dst] -> Cond (r, c, (readInt val), dst)
    else Proc s

aoc202319 input = (part1, part2) where
  [ws, ps] = input |> lines |> split [""]
  wfs = ws |> map parseWorkflow |> M.fromList
  parts = ps |> map parsePart

  --------------- PART 1 ---------------------------------------
  procRules1 :: [Rule] -> Part -> Int
  procRules1 [Proc dst] part = procDest1 dst part
  procRules1 (Cond (ratingCat, op, val, dst):rules) part = let
      rating = (part M.! ratingCat)
      isMatching = case op of
                '<' -> rating < val
                '>' -> rating > val
      in if isMatching
         then procDest1 dst part
         else procRules1 rules part

  procDest1 :: String -> Part -> Int
  procDest1 dst part = case dst of
      "A" -> part |> M.elems |> sum
      "R" -> 0
      _   -> procRules1 (wfs M.! dst) part

  --------------- PART 2 ---------------------------------------
  procRules2 :: [Rule] -> Ranges -> Int
  procRules2 [rule@(Proc dst)] ranges = procDest2 dst ranges
  procRules2 (rule@(Cond (ratingCat, op, val, dst)):rules) ranges = let
      (lowerBound, upperBound) = (ranges M.! ratingCat)
      (trueBounds, falseBounds) = case op of
        '<' -> ( (lowerBound, min upperBound (val - 1))
               , (max lowerBound val, upperBound))
        '>' -> ((max lowerBound (val + 1), upperBound)
               , (lowerBound, val))
      trueRanges = M.insert ratingCat trueBounds ranges
      trueBranch = procDest2 dst trueRanges

      falseRanges = M.insert ratingCat falseBounds ranges
      falseBranch = procRules2 rules falseRanges
      in trueBranch + falseBranch

  procDest2 :: String -> Ranges -> Int
  procDest2 dst ranges = case dst of
      "A" -> ranges |> M.elems |> map (\(lb, ub) -> ub - lb + 1) |> product
      "R" -> 0
      _   -> procRules2 (wfs M.! dst) ranges

  part1 = parts |> map (procRules1 (wfs M.! "in")) |> sum
  part2 = [(p, (1, 4000)) | p <- "xmas"] |> M.fromList |> procRules2 (wfs M.! "in")
