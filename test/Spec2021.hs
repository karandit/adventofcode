module Spec2021
( tests
) where

import Test.HUnit
import AoC2021.Day01
import AoC2021.Day02
import AoC2021.Day03
import AoC2021.Day04
import AoC2021.Day05
import AoC2021.Day06
import AoC2021.Day07
import AoC2021.Day08
import AoC2021.Day09
import AoC2021.Day10
import AoC2021.Day11
import AoC2021.Day13
import AoC2021.Day14
import AoC2021.Day16

assertAoC day aocFunc sampleAns myAns = TestList
    [ TestCase $ do
        input <- readFile ("inputs/2021/day" ++ day ++ "_sample.txt")
        assertEqual ("AoC 2021/" ++ day ++ " sample") sampleAns (aocFunc input)
    , TestCase $ do
        input <- readFile ("inputs/2021/day" ++ day ++ ".txt")
        assertEqual ("AoC 2021/" ++ day) myAns (aocFunc input)
    ]

day13BigO = [
    "xxxxx",
    "x   x",
    "x   x",
    "x   x",
    "xxxxx"]

day13BLHFJPJF = [
    "xxx  x    x  x xxxx   xx xxx    xx xxxx",
    "x  x x    x  x x       x x  x    x x   ",
    "xxx  x    xxxx xxx     x x  x    x xxx ",
    "x  x x    x  x x       x xxx     x x   ",
    "x  x x    x  x x    x  x x    x  x x   ",
    "xxx  xxxx x  x x     xx  x     xx  x   "]

tests = TestList
    [ assertAoC "01" aoc202101 (7, 5) (1532, 1571)
    , assertAoC "02" aoc202102 (150, 900) (1635930, 1781819478)
    , assertAoC "03" aoc202103 (198, 230) (3633500, 4550283)
    , assertAoC "04" aoc202104 (4512, 1924) (33462, 30070)
    , assertAoC "05" aoc202105 (5, 12) (4745, 18442)
    , assertAoC "06" aoc202106 (5934, 26984457539) (356190, 1617359101538)
    , assertAoC "07" aoc202107 (37, 168) (344735, 96798233)
    , assertAoC "08" aoc202108 (26, 61229) (554, 990964)
    , assertAoC "09" aoc202109 (15, 1134) (439, 900900)
    , assertAoC "10" aoc202110 (26397, 288957) (411471, 3122628974)
    , assertAoC "11" aoc202111 (1656, 195) (1647, 348)
    , assertAoC "13" aoc202113 (17, day13BigO) (712, day13BLHFJPJF)
    , assertAoC "14" aoc202114 (1588, 2188189693529) (2549, 2516901104210)
    , assertAoC "16" aoc202116 (31, 54) (993, 144595909277)
    ]
