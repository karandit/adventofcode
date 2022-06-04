module AoCAsserts
  ( assertAoC,
  )
where

import Test.Tasty
import Test.Tasty.HUnit

assertAoC :: (Eq a, Show a) => String -> String -> (String -> a) -> a -> TestTree
assertAoC year day aocFunc answer = testCase ("Day " ++ day) $ do
  let path = year ++ "/day" ++ day
  input <- readFile ("inputs/" ++ path ++ ".txt")
  assertEqual ("AoC " ++ path) answer (aocFunc input)
