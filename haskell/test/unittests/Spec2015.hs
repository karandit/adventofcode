module Spec2015
  ( tests,
  )
where

import AoC2015.Day01
import AoC2015.Day02
import AoC2015.Day03
import AoC2015.Day05
import AoC2015.Day06
import AoCAsserts (assertAoC)
import Test.Tasty
import Test.Tasty.HUnit

assert2015 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestTree
assert2015 sut inputs =
    let day = take 2 $ fst $ head $ inputs
    in testGroup ("Day " ++ day) $
        map (\(fileName, expected) -> assertAoC "2015" fileName sut expected) inputs

tests =
  testGroup
    "2015"
    [ assert2015 aoc201501 [("01", (138, 1771))],
      assert2015 aoc201502 [("02", (1586300, 3737498))],
      assert2015 aoc201503 [("03", (2081, 2341))],
      assert2015 aoc201505 [("05", (255, 55)), ("05S", (2, 0))],
      assert2015 aoc201506 [("06", (377891, 14110788))]
    ]
