module AoCAsserts
( assertAoC
) where

import Test.HUnit

assertAoC :: (Eq a, Show a) => String -> String -> (String -> a) -> [a] -> Test
assertAoC year day aocFunc answers =
    let suffixes = ["", "_sample"]

        testCaseAoC answer suffix = TestCase $ do
            let path = year ++ "/day" ++ day ++ suffix
            input <- readFile ("inputs/" ++ path ++ ".txt")
            assertEqual ("AoC " ++ path) answer (aocFunc input)

    in TestList $ zipWith testCaseAoC answers suffixes
