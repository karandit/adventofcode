module Spec2021
( tests
) where

import Test.HUnit
import AoC2021.Day01
import AoC2021.Day02
import AoC2021.Day03
import AoC2021.Day04

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
          , assertAoC "03" aoc202103 (198, 230) (3633500, 4550283)
          , assertAoC "04" aoc202104 (4512, 1924) (33462, 30070)
          ]
