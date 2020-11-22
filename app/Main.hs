module Main where

import AoC15.Day01

main :: IO ()
main = do
    input <- readFile "input201501.txt"
    let gold = aoc201501a input
    let silver = aoc201501b input
    print (gold, silver)
