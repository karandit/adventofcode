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

assert2021 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestTree
assert2021 sut inputs =
    let day = take 2 $ fst $ head $ inputs
    in testGroup ("Day " ++ day) $
        map (\(fileName, expected) -> assertAoC "2021" fileName sut expected) inputs

exp202113S =
  [ "xxxxx",
    "x   x",
    "x   x",
    "x   x",
    "xxxxx"
  ]

exp202113 =
  [ "xxx  x    x  x xxxx   xx xxx    xx xxxx",
    "x  x x    x  x x       x x  x    x x   ",
    "xxx  x    xxxx xxx     x x  x    x xxx ",
    "x  x x    x  x x       x xxx     x x   ",
    "x  x x    x  x x    x  x x    x  x x   ",
    "xxx  xxxx x  x x     xx  x     xx  x   "
  ]

tests =
  testGroup
    "Year 2021"
    [ assert2021 aoc202101 [("01S", (7, 5)), ("01", (1532, 1571))],
      assert2021 aoc202102 [("02S", (150, 900)), ("02", (1635930, 1781819478))],
      assert2021 aoc202103 [("03S", (198, 230)), ("03", (3633500, 4550283))],
      assert2021 aoc202104 [("04S", (4512, 1924)), ("04", (33462, 30070))],
      assert2021 aoc202105 [("05S", (5, 12)), ("05", (4745, 18442))],
      assert2021 aoc202106 [("06S", (5934, 26984457539)), ("06", (356190, 1617359101538))],
      assert2021 aoc202107 [("07S", (37, 168)), ("07", (344735, 96798233))],
      assert2021 aoc202108 [("08S", (26, 61229)), ("08", (554, 990964))],
      assert2021 aoc202109 [("09S", (15, 1134)), ("09", (439, 900900))],
      assert2021 aoc202110 [("10S", (26397, 288957)), ("10", (411471, 3122628974))],
      assert2021 aoc202111 [("11S", (1656, 195)), ("11", (1647, 348))],
      assert2021 aoc202112 [("12S", (10, 36)), ("12M", (19, 103)), ("12L", (226, 3509)), ("12", (4241, 122134))],
      assert2021 aoc202113 [("13S", (17, exp202113S)), ("13", (712, exp202113))],
      assert2021 aoc202114 [("14S", (1588, 2188189693529)), ("14", (2549, 2516901104210))],
      assert2021 aoc202115 [("15S", (40, 315)), ("15", (739, 3040))],
      assert2021 aoc202116 [("16S", (31, 54)), ("16", (993, 144595909277))],
      assert2021 aoc202117 [("17S", (45, 112)), ("17", (12561, 3785))],
      assert2021 aoc202118 [("18S", (4140, 3993)), ("18", (4173, 4706))],
      assert2021 aoc202125 [("25S", 58), ("25", 474)]
    ]
