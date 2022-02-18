module AoC2015.Day06
( aoc201506
) where

import Data.Array.MArray (newArray, readArray, writeArray, getElems)
import Data.Array.ST (runSTUArray, STUArray)
import qualified Data.Array.IArray as IA
import Control.Monad
import Control.Monad.ST (ST, runST)

import Data.Foldable (for_)
import Data.List.Utils (split)
import Utils (readInt, (|>))

data LightInstr = TurnOn | TurnOff | Toggle

rangeXY (aX, aY) (bX, bY) = [(x,y) | x <- [aX..bX], y <- [aY..bY]]
{-# INLINE rangeXY #-}

aoc201506 input = (part1, part2) where
    inputs = input |> lines |> map parseInstr
    parseInstr ('t':'u':'r':'n':' ':'o':'n':' ':s) = parseCoords TurnOn s
    parseInstr ('t':'u':'r':'n':' ':'o':'f':'f':' ':s) = parseCoords TurnOff s
    parseInstr ('t':'o':'g':'g':'l':'e':' ':s) = parseCoords Toggle s
    parseCoords instr s = let [aXY, bXY] = s |> split " through "
                              [aX, aY] = aXY |> split "," |> map readInt
                              [bX, bY] = bXY |> split "," |> map readInt
                          in (instr, (aX, aY), (bX, bY))

    solveBy :: (LightInstr -> (Bool,  Int -> Int)) -> Int
    solveBy f = runST $ do
                          arr <- newGrid
                          for_ inputs (\(instr, (aX, aY), (bX, bY)) ->
                                 for_ (rangeXY (aX,aY)(bX,bY)) (\xy ->  do
                                                let (needsRead, transform) = f instr
                                                v <- if needsRead then readArray arr xy else pure 0
                                                writeArray arr xy (transform v)
                                                )
                            )
                          es <- getElems arr
                          return $! sum es

    newGrid :: ST s (STUArray s (Int, Int) Int)
    newGrid = newArray ((0, 0), (999, 999)) 0

    rule1 TurnOn  = (False, \_ -> 1)
    rule1 TurnOff = (False, \_ -> 0)
    rule1 Toggle  = (True, \x -> 1 - x)
    part1 = solveBy rule1

    rule2 TurnOn  = (True, \x -> x + 1)
    rule2 TurnOff = (True, \x -> (x - 1) |> max 0)
    rule2 Toggle  = (True, \x -> x + 2)
    part2 = solveBy rule2
