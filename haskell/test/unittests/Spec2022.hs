module Spec2022
  ( tests,
  )
where

import AoC2022.Day01
import AoCAsserts (assertAoC)
import Test.Tasty
import Test.Tasty.HUnit

assertAoC2022 :: (Eq a, Show a) => String -> (String -> a) -> a -> TestTree
assertAoC2022 = assertAoC "2022"

tests =
  testGroup
    "2022"
    [ assertAoC2022 "01_sample" aoc202201 (24000, 45000),
      assertAoC2022 "01" aoc202201 (72240, 210957)
    ]
