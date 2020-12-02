module Spec2015
( tests
) where

import Test.HUnit
import AoC2015.Day01
import AoC2015.Day02
import AoC2015.Day03

tests = TestList
            [ testAoC201501a
            , testAoC201501b
            , testAoC201502a
            , testAoC201502b
            , testAoC201503a
            , testAoC201503b
            ]

testAoC201501a = TestList
            [ 0 ~=? (aoc201501a "(())")
            , 0 ~=? (aoc201501a "()()")
            , 3 ~=? (aoc201501a "(()(()(")
            , 3 ~=? (aoc201501a "))(((((")
            , (-1)~=? (aoc201501a "))(")
            , (-3)~=? (aoc201501a ")())())")
            , TestCase $ do
                input <- readFile "test/input201501.txt"
                assertEqual "AoC 2015/01a" 138 (aoc201501a input)
            ]

testAoC201501b = TestList
            [ 1 ~=? (aoc201501b ")")
            , 5 ~=? (aoc201501b "()())")
            , TestCase $ do
                input <- readFile "test/input201501.txt"
                assertEqual "AoC 2015/01b" 1771 (aoc201501b input)
            ]

testAoC201502a = TestList
            [ 58 ~=? (aoc201502a [2,3,4])
            , 43 ~=? (aoc201502a [1,1,10])
            , TestCase $ do
                input <- readFile "test/input201502.txt"
                let triples = parseInput201502 input
                assertEqual "AoC 2015/02a" 1586300 (sum $ map aoc201502a triples)
            ]

testAoC201502b = TestList
            [ 34 ~=? (aoc201502b [2,3,4])
            , 14 ~=? (aoc201502b [1,1,10])
            , TestCase $ do
                input <- readFile "test/input201502.txt"
                let triples = parseInput201502 input
                assertEqual "AoC 2015/02b" 3737498 (sum $ map aoc201502b triples)
            ]

testAoC201503a = TestList
            [ 2 ~=? (aoc201503a ">")
            , 4 ~=? (aoc201503a "^>v<")
            , 2 ~=? (aoc201503a "^v^v^v^v^v")
            , TestCase $ do
                input <- readFile "test/input201503.txt"
                assertEqual "AoC 2015/03a" 2081 (aoc201503a input)
            ]

testAoC201503b = TestList
            [ 3 ~=? (aoc201503b "^v")
            , 3 ~=? (aoc201503b "^>v<")
            , 11 ~=? (aoc201503b "^v^v^v^v^v")
            , TestCase $ do
                input <- readFile "test/input201503.txt"
                assertEqual "AoC 2015/03b" 2341 (aoc201503b input)
            ]
