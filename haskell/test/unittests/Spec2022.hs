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
import AoCAsserts (assertAoC)
import Test.Tasty
import Test.Tasty.HUnit

assertAoC2022 :: (Eq a, Show a) => String -> (String -> a) -> a -> TestTree
assertAoC2022 = assertAoC "2022"

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

tests =
  testGroup
    "2022"
    [ assertAoC2022 "01_sample" aoc202201 (24000, 45000),
      assertAoC2022 "01" aoc202201 (72240, 210957),
      assertAoC2022 "02_sample" aoc202202 (15, 12),
      assertAoC2022 "02" aoc202202 (11841, 13022),
      assertAoC2022 "03_sample" aoc202203 (157, 70),
      assertAoC2022 "03" aoc202203 (7824, 2798),
      assertAoC2022 "04_sample" aoc202204 (2, 4),
      assertAoC2022 "04" aoc202204 (496, 847),
      assertAoC2022 "05_sample" aoc202205 ("CMZ", "MCD"),
      assertAoC2022 "05" aoc202205 ("RFFFWBPNS", "CQQBBJFCS"),
      assertAoC2022 "06_sample" aoc202206 ([7, 5, 6, 10, 11], [19, 23, 23, 29, 26]),
      assertAoC2022 "06" aoc202206 ([1100], [2421]),
      assertAoC2022 "07_sample" aoc202207 (95437, 24933642),
      assertAoC2022 "07" aoc202207 (1770595, 2195372),
      assertAoC2022 "08_sample" aoc202208 (21, 8),
      assertAoC2022 "08" aoc202208 (1719, 590824),
      assertAoC2022 "09_sample" aoc202209 (13, 1),
      assertAoC2022 "09_large" aoc202209 (88, 36),
      assertAoC2022 "09" aoc202209 (6498, 2531),
      assertAoC2022 "10_sample" aoc202210 (13140, day10Sample),
      assertAoC2022 "10" aoc202210 (15140, day10BPJAZGAP)
    ]
