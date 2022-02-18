module Spec2015
( tests
) where

import Test.HUnit
import AoCAsserts (assertAoC)
import AoC2015.Day01
import AoC2015.Day02
import AoC2015.Day03
import AoC2015.Day05
import AoC2015.Day06

assertAoC2015 = assertAoC "2015"

tests = TestList
            [ 0 ~=? (aoc201501a "(())")
            , 0 ~=? (aoc201501a "()()")
            , 3 ~=? (aoc201501a "(()(()(")
            , 3 ~=? (aoc201501a "))(((((")
            , (-1)~=? (aoc201501a "))(")
            , (-3)~=? (aoc201501a ")())())")

            , 1 ~=? (aoc201501b ")")
            , 5 ~=? (aoc201501b "()())")
            , assertAoC2015 "01" aoc201501 (138, 1771) (138, 1771) -- sample is the same as the real input

            , 58 ~=? (aoc201502a [2,3,4])
            , 43 ~=? (aoc201502a [1,1,10])

            , 34 ~=? (aoc201502b [2,3,4])
            , 14 ~=? (aoc201502b [1,1,10])
            , assertAoC2015 "02" aoc201502 (1586300, 3737498) (1586300, 3737498) -- sample is the same as the real input

            , 2 ~=? (aoc201503a ">")
            , 4 ~=? (aoc201503a "^>v<")
            , 2 ~=? (aoc201503a "^v^v^v^v^v")

            , 3 ~=? (aoc201503b "^v")
            , 3 ~=? (aoc201503b "^>v<")
            , 11 ~=? (aoc201503b "^v^v^v^v^v")
            , assertAoC2015 "03" aoc201503 (2081, 2341) (2081, 2341) -- sample is the same as the real input
            , assertAoC2015 "05" aoc201505 (2, 0) (255, 55)
            , assertAoC2015 "06" aoc201506 (184127, 348647) (377891, 14110788) -- too slow & the sample is just the made by me
            ]
