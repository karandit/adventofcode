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

assertAoC2020 :: (Eq a, Show a) => String -> (String -> a) -> a -> Test
assertAoC2020 = assertAoC "2020"

tests = TestList
          [ 514579 ~=? (aoc202001a [1721,979,366,299, 675, 1456])               -- Day01
          , 241861950 ~=? (aoc202001b [1721,979,366,299, 675, 1456])
          , assertAoC2020 "01" aoc202001 (1007104, 18847752)
          , 1 ~=? (aoc202002a [((1,3), 'a', "abcde")])                          -- Day02
          , 0 ~=? (aoc202002a [((1,3), 'b', "cdefg")])
          , 1 ~=? (aoc202002a [((2,9), 'c', "ccccccccc")])
          , 1 ~=? (aoc202002b [((1,3), 'a', "abcde")])
          , 0 ~=? (aoc202002b [((1,3), 'b', "cdefg")])
          , 0 ~=? (aoc202002b [((2,9), 'c', "ccccccccc")])
          , assertAoC2020 "02" aoc202002 (603, 404)
          , assertAoC2020 "03" aoc202003 (191, 1478615040)                      -- Day03
          , assertAoC2020 "03_sample" aoc202003 (7, 336)
          , assertAoC2020 "04_sample" (fst . aoc202004) 2                       -- Day04
          , assertAoC2020 "04_invalid" (snd . aoc202004) 0
          , assertAoC2020 "04_valid" (snd . aoc202004) 4
          , assertAoC2020 "04" aoc202004 (222, 140)
          , 357 ~=? (seatId "FBFBBFFRLR")                                       -- Day05
          , assertAoC2020 "05" aoc202005 (880, 731)
          , assertAoC2020 "06_sample" aoc202006 (11, 6)                         -- Day06
          , assertAoC2020 "06" aoc202006 (6683, 3122)
          , assertAoC2020 "07" aoc202007 (179, 18925)                           -- Day07
          , assertAoC2020 "07_sample1" (fst . aoc202007) 4
          , assertAoC2020 "07_sample2" (snd . aoc202007) 126
          , assertAoC2020 "08_sample" aoc202008 (5, 8)                          -- Day08
          , assertAoC2020 "08" aoc202008 (2014, 2251)
          , assertAoC2020 "09_sample" (aoc202009 5) (127, 62)                   -- Day09
          , assertAoC2020 "09" (aoc202009 25) (1639024365, 219202240)
          , assertAoC2020 "10_sample1" aoc202010 (35, 8)                        -- Day10
          , assertAoC2020 "10_sample2" (snd . aoc202010) 19208
          , assertAoC2020 "10" aoc202010 (1625, 3100448333024)
          , assertAoC2020 "11_sample" aoc202011 (37, 26)                        -- Day11
          , assertAoC2020 "11" aoc202011 (2247, 2011)
          , assertAoC2020 "12_sample" aoc202012 (25, 286)                       -- Day12
          , assertAoC2020 "12" aoc202012 (1603, 52866)
          , assertAoC2020 "13_sample" aoc202013 (295, 1068781)                  -- Day13
          , assertAoC2020 "13" aoc202013 (174, 780601154795940)
          , assertAoC2020 "14_sample1" (fst . aoc202014) 165                    -- Day14
          , assertAoC2020 "14_sample2" (snd . aoc202014) 208
          , assertAoC2020 "14" aoc202014 (13496669152158, 3278997609887)
          ,    1 ~=? (aoc202015a 2020 [1,3,2])                                  -- Day15
          ,   10 ~=? (aoc202015a 2020 [2,1,3])
          ,   27 ~=? (aoc202015a 2020 [1,2,3])
          ,   78 ~=? (aoc202015a 2020 [2,3,1])
          ,  438 ~=? (aoc202015a 2020 [3,2,1])
          , 1836 ~=? (aoc202015a 2020 [3,1,2])
          , 1259 ~=? (aoc202015a 2020 [15,5,1,4,7,0])
          --,  689 ~=? (aoc202015b 30000000 [15,5,1,4,7,0]) -- TODO it is slow, see Go impl
          , assertAoC2020 "16_invalid" (fst . aoc202016) 71                     -- Day16
          , assertAoC2020 "16" aoc202016 (23044, 3765150732757)
          , assertAoC2020 "17_sample" aoc202017 (112, 848)                      -- Day17
          , assertAoC2020 "17" aoc202017 (257, 2532)
          ,     51 ~=? (calc1 "1 + (2 * 3) + (4 * (5 + 6))")                    -- Day18
          ,     26 ~=? (calc1 "2 * 3 + (4 * 5)")
          ,    437 ~=? (calc1 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
          ,  12240 ~=? (calc1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
          ,  13632 ~=? (calc1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
          ,     51 ~=? (calc2 "1 + (2 * 3) + (4 * (5 + 6))")
          ,     46 ~=? (calc2 "2 * 3 + (4 * 5)")
          ,   1445 ~=? (calc2 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
          , 669060 ~=? (calc2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
          ,  23340 ~=? (calc2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
          , assertAoC2020 "18" aoc202018 (86311597203806, 276894767062189)
          , assertAoC2020 "21_sample" aoc202021 (5, "mxmxvkd,sqjhc,fvjkl")      -- Day21
          , assertAoC2020 "21" aoc202021 (2485, "bqkndvb,zmb,bmrmhm,snhrpv,vflms,bqtvr,qzkjrtl,rkkrx")
          , assertAoC2020 "22_sample" aoc202022 (306, 291)                      -- Day22
          , assertAoC2020 "22" aoc202022 (31957, 33212)
          , ("67384529", "149245887792") ~=? (aoc202023 "389125467")            -- Day23
          , ("29385746", "680435423892") ~=? (aoc202023 "712643589")
          , assertAoC2020 "24_sample" aoc202024 (10, 2208)                      -- Day24
          , assertAoC2020 "24" aoc202024 (232, 3519)
          ]
