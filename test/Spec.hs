import Test.HUnit
import AoC15.Day01

--main :: IO ()
main = runTestTT tests

tests = TestList
            [ testAoC201501a
            , testAoC201501b
            ]

testAoC201501a = TestList
            [ "AoC 2015/01a" ~:  0 ~=? (aoc201501a "(())")
            , "AoC 2015/01a" ~:  0 ~=? (aoc201501a "()()")
            , "AoC 2015/01a" ~:  3 ~=? (aoc201501a "(()(()(")
            , "AoC 2015/01a" ~:  3 ~=? (aoc201501a "))(((((")
            , "AoC 2015/01a" ~:(-1)~=? (aoc201501a "))(")
            , "AoC 2015/01a" ~:(-3)~=? (aoc201501a ")())())")
            , TestCase $ do
                input <- readFile "/home/tomi/code/github.com/karandit/adventofcode/test/input201501.txt"
                assertEqual "AoC 2015/01a" 138 (aoc201501a input)
            ]

testAoC201501b = TestList
            [ "AoC 2015/01b" ~: 1 ~=? (aoc201501b ")")
            , "AoC 2015/01b" ~: 5 ~=? (aoc201501b "()())")
            , TestCase $ do
                input <- readFile "/home/tomi/code/github.com/karandit/adventofcode/test/input201501.txt"
                assertEqual "AoC 2015/01b" 1771 (aoc201501b input)
            ]
