import Test.HUnit
import AoC15.Day01
import AoC15.Day02

main = runTestTT tests

tests = TestList
            [ testAoC201501a
            , testAoC201501b
            , testAoC201502a
            , testAoC201502b
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
            , 42 ~=? (aoc201502a [1,1,10])
            , TestCase $ do
                input <- readFile "test/input201502.txt"
                let triples = parseInput201502 input
                assertEqual "AoC 2015/01a" 1586300 (sum $ map aoc201502a triples)
            ]

testAoC201502b = TestList
            [ 34 ~=? (aoc201502b [2,3,4])
            , 14 ~=? (aoc201502b [1,1,10])
            , TestCase $ do
                input <- readFile "test/input201502.txt"
                let triples = parseInput201502 input
                assertEqual "AoC 2015/01b" 3737498 (sum $ map aoc201502b triples)
            ]
