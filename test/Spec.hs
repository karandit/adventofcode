import Test.HUnit
import AoC15.Day01

main = runTestTT tests

tests = TestList
            [ testAoC201501a
            , testAoC201501b
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
