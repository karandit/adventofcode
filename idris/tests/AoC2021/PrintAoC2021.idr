module Main

import Data.Maybe
import Data.String
import System.File

import AoC2021.Day01

printSolution : (String -> Maybe String) -> String -> IO ()
printSolution solutionFunc day = do
         Right input <- readFile ("../../../inputs/2021/day" ++ day ++ ".txt")
         | Left err => putStrLn $ show err
         putStrLn $ "Day " ++ (padRight 15 ' ' day) ++ ": " ++ (fromMaybe (Delay "MISSING") $ solutionFunc input)

partial
main : IO ()
main = do
         printSolution aoc202101 "01S"
         printSolution aoc202101 "01"
