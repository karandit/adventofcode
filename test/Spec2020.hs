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
          ]
