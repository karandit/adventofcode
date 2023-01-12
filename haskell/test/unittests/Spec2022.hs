module Spec2022
  ( tests,
  )
where

import AoC2022.Day01
import AoC2022.Day02
import AoC2022.Day03
import AoC2022.Day04
import AoC2022.Day05
import AoC2022.Day06
import AoC2022.Day07
import AoC2022.Day08
import AoC2022.Day09
import AoC2022.Day10
import AoC2022.Day11
import AoC2022.Day12
import AoC2022.Day13
import AoCAsserts (assertAoC)
import Test.Tasty
import Test.Tasty.HUnit

assert2022 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestTree
assert2022 sut inputs =
    let day = take 2 $ fst $ head $ inputs
    in testGroup ("Day " ++ day) $
        map (\(fileName, expected) -> assertAoC "2022" fileName sut expected) inputs

day10Sample =
  [ "##..##..##..##..##..##..##..##..##..##..",
    "###...###...###...###...###...###...###.",
    "####....####....####....####....####....",
    "#####.....#####.....#####.....#####.....",
    "######......######......######......####",
    "#######.......#######.......#######....."
  ]

day10BPJAZGAP =
  [ "###..###....##..##..####..##...##..###..",
    "#..#.#..#....#.#..#....#.#..#.#..#.#..#.",
    "###..#..#....#.#..#...#..#....#..#.#..#.",
    "#..#.###.....#.####..#...#.##.####.###..",
    "#..#.#....#..#.#..#.#....#..#.#..#.#....",
    "###..#.....##..#..#.####..###.#..#.#...."
  ]

tests :: TestTree
tests =
  testGroup
    "2022"
    [ assert2022 aoc202201 [("01S", (24000, 45000)), ("01", (72240, 210957))],
      assert2022 aoc202202 [("02S", (15, 12)), ("02", (11841, 13022))],
      assert2022 aoc202203 [("03S", (157, 70)), ("03", (7824, 2798))],
      assert2022 aoc202204 [("04S", (2, 4)), ("04", (496, 847))],
      assert2022 aoc202205 [("05S", ("CMZ", "MCD")), ("05", ("RFFFWBPNS", "CQQBBJFCS"))],
      assert2022 aoc202206 [("06S", ([7, 5, 6, 10, 11], [19, 23, 23, 29, 26])), ("06", ([1100], [2421]))],
      assert2022 aoc202207 [("07S", (95437, 24933642)), ("07", (1770595, 2195372))],
      assert2022 aoc202208 [("08S", (21, 8)), ("08", (1719, 590824))],
      assert2022 aoc202209 [("09S", (13, 1)), ("09L", (88, 36)), ("09", (6498, 2531))],
      assert2022 aoc202210 [("10S", (13140, day10Sample)), ("10", (15140, day10BPJAZGAP))],
      assert2022 aoc202211 [("11S", (10605, 2713310158)), ("11", (112221, 25272176808))],
      assert2022 aoc202212 [("12S", (31, 29)), ("12", (425, 418))],
      assert2022 aoc202213 [("13S", (13, 140)), ("13", (6086, 27930))]
    ]
