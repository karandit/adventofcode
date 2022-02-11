module AoC2015.Day06
( aoc201506
) where

import qualified Data.Map as M
import Data.List (foldl')
import Data.List.Utils (split)
import Utils (readInt, (|>))

main :: IO ()
main = do 
    input <- readFile "inputs/2015/day06.txt"
    putStrLn $ show $ aoc201506 input

data LightInstr = TurnOn | TurnOff | Toggle

aoc201506 input = (part1, part2) where
    inputs = input |> lines |> map parseInstr
    parseInstr ('t':'u':'r':'n':' ':'o':'n':' ':s) = parseCoords TurnOn s
    parseInstr ('t':'u':'r':'n':' ':'o':'f':'f':' ':s) = parseCoords TurnOff s
    parseInstr ('t':'o':'g':'g':'l':'e':' ':s) = parseCoords Toggle s
    parseCoords state s = let [aXY, bXY] = s |> split " through "
                              [aX, aY] = aXY |> split "," |> map readInt
                              [bX, bY] = bXY |> split "," |> map readInt
                          in (state, (aX, aY), (bX, bY))

    initGrid initV= [((x, y), initV) | x <- [0..999], y <- [0..999]] |> M.fromDistinctAscList
    consumeInstr f grid (state, (aX, aY), (bX, bY)) = [(x,y)| x<-[aX..bX], y<-[aY..bY]]
                        |> foldl' (\accGrid xy -> M.adjust (f state) xy accGrid) grid

    applyInstr initV f = inputs |> foldl' (consumeInstr f) (initGrid initV)

    instr1 TurnOn = const True
    instr1 TurnOff = const False
    instr1 Toggle = not
    part1 = applyInstr False instr1 |> M.elems |> filter id |> length

    instr2 TurnOn = \x -> x + 1
    instr2 TurnOff = \x -> (x - 1) |> max 0
    instr2 Toggle = \x -> x + 2
    part2 = applyInstr 0 instr2 |> M.elems |> sum
