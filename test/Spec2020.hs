module Spec2020
( tests
) where

import Test.HUnit
import AoC2020.Day01
import AoC2020.Day02
import AoC2020.Day03
import AoC2020.Day04

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
--                input <- readFile "test/input202004_sample.txt"
--                assertEqual "AoC 2020/04a" 2 (aoc202004a input)
--           , TestCase $ do
                input <- readFile "test/input202004.txt"
                assertEqual "AoC 2020/04a" 222 (aoc202004a input)
            ]
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202004_invalid.txt"
                assertEqual "AoC 2020/04b" 0 (aoc202004b input)
--            , TestCase $ do
--                input <- readFile "test/input202004_valid.txt"
--                assertEqual "AoC 2020/04b" 4 (aoc202004b input)
            , TestCase $ do
                input <- readFile "test/input202004.txt"
                assertEqual "AoC 2020/04b" 140 (aoc202004b input)
            ]
          ]

