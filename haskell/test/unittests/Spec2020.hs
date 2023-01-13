module Spec2020
  ( tests,
  )
where

import AoC2020.Day01
import AoC2020.Day02
import AoC2020.Day03
import AoC2020.Day04
import AoC2020.Day05
import AoC2020.Day06
import AoC2020.Day07
import AoC2020.Day08
import AoC2020.Day09
import AoC2020.Day10
import AoC2020.Day11
import AoC2020.Day12
import AoC2020.Day13
import AoC2020.Day14
import AoC2020.Day15
import AoC2020.Day16
import AoC2020.Day17
import AoC2020.Day18
import AoC2020.Day19
import AoC2020.Day20
import AoC2020.Day21
import AoC2020.Day22
import AoC2020.Day23
import AoC2020.Day24
import AoC2020.Day25
import AoCAsserts (assertAoC)
import Test.Tasty
import Test.Tasty.HUnit

assert2020 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestTree
assert2020 sut inputs =
    let day = take 2 $ fst $ head $ inputs
    in testGroup ("Day " ++ day) $
        map (\(fileName, expected) -> assertAoC "2020" fileName sut expected) inputs

tests =
  testGroup
    "Year 2020"
    [ assert2020 aoc202001 [("01", (1007104, 18847752))],
      assert2020 aoc202002 [("02", (603, 404))],
      assert2020 aoc202003 [("03", (191, 1478615040)), ("03S", (7, 336))],
      assert2020 aoc202004 [("04", (222, 140)), ("04S", (2, 2)), ("04M", (4, 0)), ("04L", (4, 4))],
      assert2020 aoc202005 [("05", (880, 731))],
      assert2020 aoc202006 [("06", (6683, 3122)), ("06S", (11, 6))],
      assert2020 aoc202007 [("07", (179, 18925)), ("07S", (4, 32))],
      assert2020 aoc202008 [("08", (2014, 2251)), ("08S", (5, 8))],
      assert2020 aoc202009 [("09", (1639024365, 219202240))],
      assert2020 aoc202010 [("10", (1625, 3100448333024)), ("10S", (35, 8)), ("10L", (220, 19208))],
      assert2020 aoc202011 [("11", (2247, 2011)), ("11S", (37, 26))],
      assert2020 aoc202012 [("12", (1603, 52866)), ("12S", (25, 286))],
      assert2020 aoc202013 [("13",  (174, 780601154795940)), ("13S", (295, 1068781))],
      assert2020 aoc202014 [("14", (13496669152158, 3278997609887))],
      assert2020 aoc202015 [("15", (1259, "MISSING"))], -- part2 is too slow, it was implemented in Go
      assert2020 aoc202016 [("16", (23044, 3765150732757))],
      assert2020 aoc202017 [("17", (257, 2532)), ("17S", (112, 848))],
      assert2020 aoc202018 [("18", (86311597203806, 276894767062189))],
      assert2020 aoc202019 [("19", "MISSING"), ("19S", "MISSING"), ("19L", "MISSING")],
      assert2020 aoc202020 [("20", (104831106565027, "MISSING")), ("20S", (20899048083289, "MISSING"))],
      assert2020 aoc202021 [("21", (2485, "bqkndvb,zmb,bmrmhm,snhrpv,vflms,bqtvr,qzkjrtl,rkkrx")), ("21S", (5, "mxmxvkd,sqjhc,fvjkl"))],
      assert2020 aoc202022 [("22", (31957, 33212)), ("22S", (306, 291))],
      assert2020 aoc202023 [("23", ("29385746", "680435423892")), ("23S", ("67384529", "149245887792"))],
      assert2020 aoc202024 [("24", (232, 3519)), ("24S", (10, 2208))],
      assert2020 aoc202025 [("25", 12285001), ("25S", 14897079)]
    ]
