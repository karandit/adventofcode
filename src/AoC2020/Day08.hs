module AoC2020.Day08
( aoc202008a
, aoc202008b
) where

import Data.List
import Utils (readInt, replaceNth)

parseOp str =
  let a:b:_ = words $ filter (/= '+') str
  in (a, readInt b)

runOp ("nop", _) = (0, 1)
runOp ("acc", n) = (n, 1)
runOp ("jmp", n) = (0, n)

getAcc ops = 
  let countOps = length ops
      runOps row visited acc op = 
           let (dAcc,dRow) = runOp op 
               (newAcc, newRow) = (acc + dAcc, (row + dRow) `mod` countOps)
           in if elem row visited 
              then (acc, head visited == countOps - 1)
              else runOps newRow (row:visited) newAcc (ops!!newRow)
    in runOps 0 [] 0 $ head ops

aoc202008a input = fst . getAcc . map parseOp . lines $ input
aoc202008b input =
        let ops = map parseOp $ lines input
        in maybe 0 fst 
           . find snd   
           . map (\(idx, (inst,n)) -> case inst of
                     "nop" -> getAcc $ replaceNth idx ("jmp", n) ops
                     "jmp" -> getAcc $ replaceNth idx ("nop", n) ops
                     "acc" -> (0, False)) 
           $ zip [0..] ops
