module Spec2021
( tests
) where

import Test.HUnit
import AoC2021.Day01

tests = TestList
          -- Day01
          [ TestList
            [ TestCase $ do
                input <- readFile "inputs/2021/day01_sample.txt"
                assertEqual "AoC 2021/01 sample" (7, 5) (aoc202101 input)
            , TestCase $ do
                input <- readFile "inputs/2021/day01.txt"
                assertEqual "AoC 2021/01" (1532, 1571) (aoc202101 input)
            ]
          ]
