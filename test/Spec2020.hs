module Spec2020
( tests
) where

import Test.HUnit
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

tests = TestList
          -- Day01
          [ TestList
            [ 514579 ~=? (aoc202001a [1721,979,366,299, 675, 1456])
            , TestCase $ do
                input <- readFile "test/input202001.txt"
                let nums = map (read :: String -> Int) $ lines input
                assertEqual "AoC 2020/01a" 1007104 (aoc202001a nums)
            ]
          , TestList
            [ 241861950 ~=? (aoc202001b [1721,979,366,299, 675, 1456])
            , TestCase $ do
                input <- readFile "test/input202001.txt"
                let nums = map (read :: String -> Int) $ lines input
                assertEqual "AoC 2020/01b" 18847752 (aoc202001b nums)
            ]
          -- Day02
          , TestList
            [ 1 ~=? (aoc202002a [((1,3), 'a', "abcde")])
            , 0 ~=? (aoc202002a [((1,3), 'b', "cdefg")])
            , 1 ~=? (aoc202002a [((2,9), 'c', "ccccccccc")])
            , TestCase $ do
                input <- readFile "test/input202002.txt"
                assertEqual "AoC 2020/02a" 603 (aoc202002a $ parseInput202002 input)
            ]
          , TestList
            [ 1 ~=? (aoc202002b [((1,3), 'a', "abcde")])
            , 0 ~=? (aoc202002b [((1,3), 'b', "cdefg")])
            , 0 ~=? (aoc202002b [((2,9), 'c', "ccccccccc")])
            , TestCase $ do
                input <- readFile "test/input202002.txt"
                assertEqual "AoC 2020/02b" 404 (aoc202002b $ parseInput202002 input)
            ]
          -- Day03
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202003_sample.txt"
                assertEqual "AoC 2020/03a" 7 (aoc202003a input)
            , TestCase $ do
                input <- readFile "test/input202003.txt"
                assertEqual "AoC 2020/03a" 191 (aoc202003a input)
            ]
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202003_sample.txt"
                assertEqual "AoC 2020/03b" 336 (aoc202003b input)
            , TestCase $ do
                input <- readFile "test/input202003.txt"
                assertEqual "AoC 2020/03b" 1478615040 (aoc202003b input)
            ]
          -- Day04
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202004_sample.txt"
                assertEqual "AoC 2020/04a sample" 2 (aoc202004a input)
           , TestCase $ do
                input <- readFile "test/input202004.txt"
                assertEqual "AoC 2020/04a" 222 (aoc202004a input)
            ]
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202004_invalid.txt"
                assertEqual "AoC 2020/04b invalid" 0 (aoc202004b input)
            , TestCase $ do
                input <- readFile "test/input202004_valid.txt"
                assertEqual "AoC 2020/04b valid" 4 (aoc202004b input)
            , TestCase $ do
                input <- readFile "test/input202004.txt"
                assertEqual "AoC 2020/04b" 140 (aoc202004b input)
            ]
          -- Day05
          , TestList
            [ 357 ~=? (seatId "FBFBBFFRLR")
            , TestCase $ do
                input <- readFile "test/input202005.txt"
                assertEqual "AoC 2020/05a" 880 (aoc202005a input)
            , TestCase $ do
                input <- readFile "test/input202005.txt"
                assertEqual "AoC 2020/05b" 731 (aoc202005b input)
            ]
          -- Day06
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202006_sample.txt"
                assertEqual "AoC 2020/06a" 11 (aoc202006a input)
            , TestCase $ do
                input <- readFile "test/input202006.txt"
                assertEqual "AoC 2020/06a" 6683 (aoc202006a input)
            , TestCase $ do
                input <- readFile "test/input202006_sample.txt"
                assertEqual "AoC 2020/06b" 6 (aoc202006b input)
            , TestCase $ do
                input <- readFile "test/input202006.txt"
                assertEqual "AoC 2020/06b" 3122 (aoc202006b input)
            ]
          -- Day07
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202007_sample1.txt"
                assertEqual "AoC 2020/07a" 4 (aoc202007a input)
            , TestCase $ do
                input <- readFile "test/input202007.txt"
                assertEqual "AoC 2020/07a" 179 (aoc202007a input)
            , TestCase $ do
                input <- readFile "test/input202007_sample2.txt"
                assertEqual "AoC 2020/07b" 126 (aoc202007b input)
            , TestCase $ do
                input <- readFile "test/input202007.txt"
                assertEqual "AoC 2020/07b" 18925 (aoc202007b input)
            ]
          -- Day08
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202008_sample.txt"
                assertEqual "AoC 2020/08a" 5 (aoc202008a input)
            , TestCase $ do
                input <- readFile "test/input202008.txt"
                assertEqual "AoC 2020/08a" 2014 (aoc202008a input)
            , TestCase $ do
                input <- readFile "test/input202008_sample.txt"
                assertEqual "AoC 2020/08b" 8 (aoc202008b input)
            , TestCase $ do
                input <- readFile "test/input202008.txt"
                assertEqual "AoC 2020/08b" 2251 (aoc202008b input)
            ]
          -- Day09
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202009_sample.txt"
                assertEqual "AoC 2020/09a" 127 (aoc202009a 5 input)
            , TestCase $ do
                input <- readFile "test/input202009.txt"
                assertEqual "AoC 2020/09a" 1639024365 (aoc202009a 25 input)
            , TestCase $ do
                input <- readFile "test/input202009_sample.txt"
                assertEqual "AoC 2020/09b" 62 (aoc202009b 5 input)
            , TestCase $ do
                input <- readFile "test/input202009.txt"
                assertEqual "AoC 2020/09b" 219202240 (aoc202009b 25 input)
            ]
          -- Day10
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202010_sample1.txt"
                assertEqual "AoC 2020/10a sample" 35 (aoc202010a input)
            , TestCase $ do
                input <- readFile "test/input202010.txt"
                assertEqual "AoC 2020/10a" 1625 (aoc202010a input)
            , TestCase $ do
                input <- readFile "test/input202010_sample1.txt"
                assertEqual "AoC 2020/10a sample" 8 (aoc202010b input)
            , TestCase $ do
                input <- readFile "test/input202010_sample2.txt"
                assertEqual "AoC 2020/10a sample long" 19208 (aoc202010b input)
            , TestCase $ do
                input <- readFile "test/input202010.txt"
                assertEqual "AoC 2020/10b" 3100448333024 (aoc202010b input)
            ]
          ]
