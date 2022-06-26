module Spec2021
  ( tests,
  )
where

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
import AoC2021.Day12
import AoC2021.Day13
import AoC2021.Day14
import AoC2021.Day15
import AoC2021.Day16
import AoC2021.Day17
import AoC2021.Day18
import AoC2021.Day25
import AoCAsserts (assertAoC)
import Test.Tasty
import Test.Tasty.HUnit

assertAoC2021 :: (Eq a, Show a) => String -> (String -> a) -> a -> TestTree
assertAoC2021 = assertAoC "2021"

day13BigO =
  [ "xxxxx",
    "x   x",
    "x   x",
    "x   x",
    "xxxxx"
  ]

day13BLHFJPJF =
  [ "xxx  x    x  x xxxx   xx xxx    xx xxxx",
    "x  x x    x  x x       x x  x    x x   ",
    "xxx  x    xxxx xxx     x x  x    x xxx ",
    "x  x x    x  x x       x xxx     x x   ",
    "x  x x    x  x x    x  x x    x  x x   ",
    "xxx  xxxx x  x x     xx  x     xx  x   "
  ]

tests =
  testGroup
    "2021"
    [ assertAoC2021 "01_sample" aoc202101 (7, 5),
      assertAoC2021 "01" aoc202101 (1532, 1571),
      assertAoC2021 "02_sample" aoc202102 (150, 900),
      assertAoC2021 "02" aoc202102 (1635930, 1781819478),
      assertAoC2021 "03_sample" aoc202103 (198, 230),
      assertAoC2021 "03" aoc202103 (3633500, 4550283),
      assertAoC2021 "04_sample" aoc202104 (4512, 1924),
      assertAoC2021 "04" aoc202104 (33462, 30070),
      assertAoC2021 "05_sample" aoc202105 (5, 12),
      assertAoC2021 "05" aoc202105 (4745, 18442),
      assertAoC2021 "06_sample" aoc202106 (5934, 26984457539),
      assertAoC2021 "06" aoc202106 (356190, 1617359101538),
      assertAoC2021 "07_sample" aoc202107 (37, 168),
      assertAoC2021 "07" aoc202107 (344735, 96798233),
      assertAoC2021 "08_sample" aoc202108 (26, 61229),
      assertAoC2021 "08" aoc202108 (554, 990964),
      assertAoC2021 "09_sample" aoc202109 (15, 1134),
      assertAoC2021 "09" aoc202109 (439, 900900),
      assertAoC2021 "10_sample" aoc202110 (26397, 288957),
      assertAoC2021 "10" aoc202110 (411471, 3122628974),
      assertAoC2021 "11_sample" aoc202111 (1656, 195),
      assertAoC2021 "11" aoc202111 (1647, 348),
      assertAoC2021 "12_small" aoc202112 (10, 36),
      assertAoC2021 "12_large1" aoc202112 (19, 103),
      assertAoC2021 "12_large2" aoc202112 (226, 3509),
      assertAoC2021 "12" aoc202112 (4241, 122134),
      assertAoC2021 "13_sample" aoc202113 (17, day13BigO),
      assertAoC2021 "13" aoc202113 (712, day13BLHFJPJF),
      assertAoC2021 "14_sample" aoc202114 (1588, 2188189693529),
      assertAoC2021 "14" aoc202114 (2549, 2516901104210),
      assertAoC2021 "15_sample" aoc202115 (40, 315),
      assertAoC2021 "15" aoc202115 (739, 3040),
      assertAoC2021 "16_sample" aoc202116 (31, 54),
      assertAoC2021 "16" aoc202116 (993, 144595909277),
      assertAoC2021 "17_sample" aoc202117 (45, 112),
      assertAoC2021 "17" aoc202117 (12561, 3785),
      assertAoC2021 "18_sample" aoc202118 (4140, 3993),
      assertAoC2021 "18" aoc202118 (4173, 4706),
      assertAoC2021 "25_sample" aoc202125 58,
      assertAoC2021 "25" aoc202125 474
    ]
