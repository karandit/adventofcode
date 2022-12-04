module Spec2022
  ( tests,
  )
where

import AoC2022.Day01
import AoC2022.Day02
import AoC2022.Day03
import AoC2022.Day04
import AoCAsserts (assertAoC)
import Test.Tasty
import Test.Tasty.HUnit

assertAoC2022 :: (Eq a, Show a) => String -> (String -> a) -> a -> TestTree
assertAoC2022 = assertAoC "2022"

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
      assertAoC2022 "04" aoc202204 (496, 847)
    ]
