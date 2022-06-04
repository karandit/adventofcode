module AoC2021.Day13
  ( aoc202113,
  )
where

import Data.List
import Data.List.Utils (split)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (readInt, (|>))

type Dot = (Int, Int)

type Dots = Set Dot

type Instr = (Char, Int)

aoc202113 :: String -> (Int, [String])
aoc202113 input = (part1, part2)
  where
    [xyLines, instrLines] = input |> lines |> split [""]

    inputDots :: Dots
    inputDots =
      xyLines
        |> map (\xyLine -> xyLine |> split "," |> map readInt |> \[xI, yI] -> (xI, yI))
        |> Set.fromList

    instrs :: [Instr]
    instrs =
      instrLines
        |> map (\('f' : 'o' : 'l' : 'd' : ' ' : 'a' : 'l' : 'o' : 'n' : 'g' : ' ' : axis : '=' : nr) -> (axis, readInt nr))

    foldDot :: Instr -> Dot -> Dot
    foldDot ('x', lx) (x, y) = (lx - (abs (x - lx)), y)
    foldDot ('y', ly) (x, y) = (x, ly - (abs (y - ly)))

    foldDots :: Dots -> Instr -> Dots
    foldDots dots instr = Set.map (foldDot instr) dots

    part1 :: Int
    part1 = instrs |> head |> foldDots inputDots |> length
    dots2 :: Dots
    dots2 = instrs |> foldl' foldDots inputDots

    maxX = dots2 |> Set.map fst |> Set.findMax
    maxY = dots2 |> Set.map snd |> Set.findMax
    part2 :: [String]
    part2 = [[if Set.member (x, y) dots2 then 'x' else ' ' | x <- [0 .. maxX]] | y <- [0 .. maxY]]
