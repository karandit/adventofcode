module AoC2023.Day15
( aoc202315
) where

import Utils (readInt, (|>))
import Data.Char (ord)
import Data.List (deleteBy, findIndex)
import Data.List.Utils (split)
import Data.Foldable (foldl')
import qualified Data.Map.Strict as M

hash = foldl' (\acc c -> ((acc + ord c) * 17) `mod` 256) 0

data Op = Rem | Ins Int deriving Show
type Action = (String, Op)

procAct boxes (lbl, act@(Ins n)) = M.insertWith (\nv ov -> 
                                if findIndex (\(lba, _) -> lba == lbl) ov /= Nothing
                                then map (\e@(lba, _) -> if lba == lbl then (lba, n) else e) ov
                                else ov ++ nv) (hash lbl) [(lbl, n)] boxes
procAct boxes (lbl, Rem)  = M.adjust (deleteBy (\(lba,_) (lbb,_) -> lba == lbb) (lbl, 0)) (hash lbl) boxes

aoc202315 input = (part1, part2) where
  inputs = input |> lines |> head |> split ","
  part1 = inputs |> map hash |> sum
  lbls = inputs |> map (\s ->
          if elem '=' s
          then s |> split "=" |> \[a, b] -> (a, Ins (readInt b))
          else s |> split "-" |> \[a, _] -> (a, Rem)
      )
  boxes = foldl' procAct M.empty lbls 
  part2 = boxes
        |> M.mapWithKey (\boxId lns -> (boxId + 1) * (sum [slotId * focal | (slotId, (_, focal)) <- zip [1..] lns]))
        |> M.elems
        |> sum
