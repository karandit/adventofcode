module Spec2020
( tests
) where

import Test.HUnit
import AoCAsserts (assertAoC)
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
import AoC2020.Day11
import AoC2020.Day12
import AoC2020.Day13
import AoC2020.Day14
import AoC2020.Day15
import AoC2020.Day16
import AoC2020.Day17
import AoC2020.Day18
import AoC2020.Day21
import AoC2020.Day22
import AoC2020.Day23
import AoC2020.Day24

assertAoC2020 = assertAoC "2020"

tests = TestList
          -- Day01
          [ 514579 ~=? (aoc202001a [1721,979,366,299, 675, 1456])
          , 241861950 ~=? (aoc202001b [1721,979,366,299, 675, 1456])
          , assertAoC2020 "01" aoc202001 [(1007104, 18847752)]
          -- Day02
          , 1 ~=? (aoc202002a [((1,3), 'a', "abcde")])
          , 0 ~=? (aoc202002a [((1,3), 'b', "cdefg")])
          , 1 ~=? (aoc202002a [((2,9), 'c', "ccccccccc")])
          , 1 ~=? (aoc202002b [((1,3), 'a', "abcde")])
          , 0 ~=? (aoc202002b [((1,3), 'b', "cdefg")])
          , 0 ~=? (aoc202002b [((2,9), 'c', "ccccccccc")])
          , assertAoC2020 "02" aoc202002 [(603, 404)]
          -- Day03
          , assertAoC2020 "03" aoc202003 [(191, 1478615040), (7, 336)]
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
          , 357 ~=? (seatId "FBFBBFFRLR")
          , assertAoC2020 "05" aoc202005 [(880, 731)]
          -- Day06
          , assertAoC2020 "06" aoc202006 [(6683, 3122), (11, 6)]
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
          , assertAoC2020 "08" aoc202008 [(2014, 2251), (5, 8)]
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
          -- Day11
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202011_sample.txt"
                assertEqual "AoC 2020/11a sample" 37 (aoc202011a input)
            , TestCase $ do
                input <- readFile "test/input202011.txt"
                assertEqual "AoC 2020/11a" 2247 (aoc202011a input)
            , TestCase $ do
                input <- readFile "test/input202011_sample.txt"
                assertEqual "AoC 2020/11b sample" 26 (aoc202011b input)
            , TestCase $ do
                input <- readFile "test/input202011.txt"
                assertEqual "AoC 2020/11b" 2011 (aoc202011b input)
            ]
          -- Day12
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202012_sample.txt"
                assertEqual "AoC 2020/12a sample" 25 (aoc202012a input)
            , TestCase $ do
                input <- readFile "test/input202012.txt"
                assertEqual "AoC 2020/12a" 1603 (aoc202012a input)
            , TestCase $ do
                input <- readFile "test/input202012_sample.txt"
                assertEqual "AoC 2020/12b sample" 286 (aoc202012b input)
            , TestCase $ do
                input <- readFile "test/input202012.txt"
                assertEqual "AoC 2020/12b" 52866 (aoc202012b input)
            ]
          -- Day13
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202013_sample.txt"
                assertEqual "AoC 2020/13a sample" 295 (aoc202013a input)
            , TestCase $ do
                input <- readFile "test/input202013.txt"
                assertEqual "AoC 2020/13a" 174 (aoc202013a input)
            , TestCase $ do
                input <- readFile "test/input202013_sample.txt"
                assertEqual "AoC 2020/13b sample" 1068781 (aoc202013b input)
            , TestCase $ do
                input <- readFile "test/input202013.txt"
                assertEqual "AoC 2020/13b" 780601154795940 (aoc202013b input)
            ]
          -- Day14
          , TestList
            [ TestCase $ do
                --input <- readFile "test/input202014_sample1.txt"
                --assertEqual "AoC 2020/14a sample" 25 (aoc202014a input)
            --, TestCase $ do
                input <- readFile "test/input202014.txt"
                assertEqual "AoC 2020/14a" 13496669152158 (aoc202014a input)
            --, TestCase $ do
                --input <- readFile "test/input202014_sample2.txt"
                --assertEqual "AoC 2020/14b sample" 286 (aoc202014b input)
            , TestCase $ do
                input <- readFile "test/input202014.txt"
                assertEqual "AoC 2020/14b" 3278997609887 (aoc202014b input)
            ]
            -- Day15
          , TestList
            [    1 ~=? (aoc202015a 2020 [1,3,2])
            ,   10 ~=? (aoc202015a 2020 [2,1,3])
            ,   27 ~=? (aoc202015a 2020 [1,2,3])
            ,   78 ~=? (aoc202015a 2020 [2,3,1])
            ,  438 ~=? (aoc202015a 2020 [3,2,1])
            , 1836 ~=? (aoc202015a 2020 [3,1,2])
            , 1259 ~=? (aoc202015a 2020 [15,5,1,4,7,0])
            --,  689 ~=? (aoc202015b 30000000 [15,5,1,4,7,0]) -- TODO it is slow, see Go impl
            ]
            -- Day16
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202016_invalid.txt"
                assertEqual "AoC 2020/16a invalid" 71 (aoc202016a input)
            , TestCase $ do
                input <- readFile "test/input202016.txt"
                assertEqual "AoC 2020/16a" 23044 (aoc202016a input)
            , TestCase $ do
                input <- readFile "test/input202016.txt"
                assertEqual "AoC 2020/16b" 3765150732757 (aoc202016b input)
            ]
            -- Day17
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202017_sample.txt"
                assertEqual "AoC 2020/17a sample" 112 (aoc202017a input)
            , TestCase $ do
                input <- readFile "test/input202017.txt"
                assertEqual "AoC 2020/17a" 257 (aoc202017a input)
            , TestCase $ do
                input <- readFile "test/input202017_sample.txt"
                assertEqual "AoC 2020/17b sample" 848 (aoc202017b input)
            , TestCase $ do
                input <- readFile "test/input202017.txt"
                assertEqual "AoC 2020/17b" 2532 (aoc202017b input)
            ]
            -- Day18
          , TestList
            [     51 ~=? (calc1 "1 + (2 * 3) + (4 * (5 + 6))")
            ,     26 ~=? (calc1 "2 * 3 + (4 * 5)")
            ,    437 ~=? (calc1 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
            ,  12240 ~=? (calc1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
            ,  13632 ~=? (calc1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
            , TestCase $ do
                input <- readFile "test/input202018.txt"
                assertEqual "AoC 2020/18a" 86311597203806 (aoc202018a input)
            ,     51 ~=? (calc2 "1 + (2 * 3) + (4 * (5 + 6))")
            ,     46 ~=? (calc2 "2 * 3 + (4 * 5)")
            ,   1445 ~=? (calc2 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
            , 669060 ~=? (calc2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
            ,  23340 ~=? (calc2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
            , TestCase $ do
                input <- readFile "test/input202018.txt"
                assertEqual "AoC 2020/18b" 276894767062189 (aoc202018b input)
            ]
            -- Day21
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202021_sample.txt"
                assertEqual "AoC 2020/21a sample" 5 (aoc202021a input)
            , TestCase $ do
                input <- readFile "test/input202021.txt"
                assertEqual "AoC 2020/21a" 2485 (aoc202021a input)
            , TestCase $ do
                input <- readFile "test/input202021_sample.txt"
                assertEqual "AoC 2020/21b sample" "mxmxvkd,sqjhc,fvjkl" (aoc202021b input)
            , TestCase $ do
                input <- readFile "test/input202021.txt"
                assertEqual "AoC 2020/21b" "bqkndvb,zmb,bmrmhm,snhrpv,vflms,bqtvr,qzkjrtl,rkkrx" (aoc202021b input)
            ]
            -- Day22
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202022_sample.txt"
                assertEqual "AoC 2020/22a sample" 306 (aoc202022a input)
            , TestCase $ do
                input <- readFile "test/input202022.txt"
                assertEqual "AoC 2020/22a" 31957 (aoc202022a input)
            , TestCase $ do
                input <- readFile "test/input202022_sample.txt"
                assertEqual "AoC 2020/22b sample" 291 (aoc202022b input)
            , TestCase $ do
                input <- readFile "test/input202022.txt"
                assertEqual "AoC 2020/22b" 33212 (aoc202022b input)
            ]
            -- Day23
          , TestList
            [     "67384529" ~=? (aoc202023a "389125467")
            ,     "29385746" ~=? (aoc202023a "712643589")
            , "149245887792" ~=? (aoc202023b "389125467")
            , "680435423892" ~=? (aoc202023b "712643589")
            ]
            -- Day24
          , TestList
            [ TestCase $ do
                input <- readFile "test/input202024_sample.txt"
                assertEqual "AoC 2020/24a sample" 10 (aoc202024a input)
            , TestCase $ do
                input <- readFile "test/input202024.txt"
                assertEqual "AoC 2020/24a" 232 (aoc202024a input)
            , TestCase $ do
                input <- readFile "test/input202024_sample.txt"
                assertEqual "AoC 2020/24b sample" 2208 (aoc202024b input)
            , TestCase $ do
                input <- readFile "test/input202024.txt"
                assertEqual "AoC 2020/24b" 3519 (aoc202024b input)
            ]
          ]
