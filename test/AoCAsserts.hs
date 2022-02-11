module AoCAsserts
( assertAoC
) where

import Test.HUnit

assertAoC year day aocFunc sampleAns myAns = TestList
    [ TestCase $ do
        input <- readFile ("inputs/" ++ year ++ "/day" ++ day ++ "_sample.txt")
        assertEqual ("AoC " ++ year ++ "/" ++ day ++ " sample") sampleAns (aocFunc input)
    , TestCase $ do
        input <- readFile ("inputs/" ++ year ++ "/day" ++ day ++ ".txt")
        assertEqual ("AoC " ++ year ++ "/" ++ day) myAns (aocFunc input)
    ]

