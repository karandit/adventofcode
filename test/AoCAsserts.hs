module AoCAsserts
( assertAoC
) where

import Test.HUnit

assertAoC :: (Eq a, Show a) => String -> String -> (String -> a) -> a -> Test
assertAoC year day aocFunc answer = TestCase $ do
            let path = year ++ "/day" ++ day
            input <- readFile ("inputs/" ++ path ++ ".txt")
            assertEqual ("AoC " ++ path) answer (aocFunc input)
