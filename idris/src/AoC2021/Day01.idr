module AoC2021.Day01

import Data.String
import Data.List
import Data.Maybe

import AoC

%default total

increasings : List Integer -> Maybe Nat
increasings nrs = 
          do tailnrs1 <- tail' nrs
             zip nrs tailnrs1 |> filter (\(a1, a2) => a2 > a1) |> length |> pure

slidings : List Integer -> Maybe (List Integer)
slidings nrs = 
          do tailnrs1 <- tail' nrs
             tailnrs2 <- tail' tailnrs1
             zip3 nrs tailnrs1 tailnrs2 |> map (\(a1, a2, a3) => a1 + a2 + a3) |> pure

export
aoc202101 : String -> Maybe String
aoc202101 input = 
         let nrs = input |> lines |> map (fromMaybe (Delay 0) . parseInteger)
         in do part1 <- increasings nrs
               sumOfSlidings <- slidings nrs 
               part2 <- increasings sumOfSlidings
               pure $ show (part1, part2)
