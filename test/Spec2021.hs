module Spec2021
( tests
) where

import Test.HUnit
import AoC2021.Day01
import AoC2021.Day02

assertAoC day aocFunc sampleAns myAns =
             TestList
            [ TestCase $ do
                input <- readFile ("inputs/2021/day" ++ day ++ "_sample.txt")
                assertEqual ("AoC 2021/" ++ day ++ " sample") sampleAns (aocFunc input)
            , TestCase $ do
                input <- readFile ("inputs/2021/day" ++ day ++ ".txt")
                assertEqual ("AoC 2021/" ++ day) myAns (aocFunc input)
            ]

tests = TestList
          [ assertAoC "01" aoc202101 (7, 5) (1532, 1571)
          , assertAoC "02" aoc202102 (150, 900) (1635930, 1781819478)
          ]
