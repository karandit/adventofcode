module Spec2015
( tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import AoCAsserts (assertAoC)
import AoC2015.Day01
import AoC2015.Day02
import AoC2015.Day03
import AoC2015.Day05
import AoC2015.Day06

assertAoC2015 :: (Eq a, Show a) => String -> (String -> a) -> a -> TestTree
assertAoC2015 = assertAoC "2015"

tests = testGroup "2015"
          [ assertAoC2015 "01" aoc201501 (138, 1771)                              -- Day01
          , assertAoC2015 "02" aoc201502 (1586300, 3737498)                       -- Day02
          , assertAoC2015 "03" aoc201503 (2081, 2341)
          , assertAoC2015 "05_sample" aoc201505 (2, 0)                          -- Day05
          , assertAoC2015 "05" aoc201505 (255, 55)
          , assertAoC2015 "06" aoc201506 (377891, 14110788)                     -- Day06
          ]
