module Spec2020
( tests
) where

import Test.Tasty
import Test.Tasty.HUnit
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
import AoC2020.Day19
import AoC2020.Day20
import AoC2020.Day21
import AoC2020.Day22
import AoC2020.Day23
import AoC2020.Day24
import AoC2020.Day25

assertAoC2020 :: (Eq a, Show a) => String -> (String -> a) -> a -> TestTree
assertAoC2020 = assertAoC "2020"

tests = testGroup "2020"
          [ assertAoC2020 "01" aoc202001 (1007104, 18847752)                    -- Day01
          , assertAoC2020 "02" aoc202002 (603, 404)                             -- Day02
          , assertAoC2020 "03" aoc202003 (191, 1478615040)                      -- Day03
          , assertAoC2020 "03_sample" aoc202003 (7, 336)
          , assertAoC2020 "04_sample" (fst . aoc202004) 2                       -- Day04
          , assertAoC2020 "04_invalid" (snd . aoc202004) 0
          , assertAoC2020 "04_valid" (snd . aoc202004) 4
          , assertAoC2020 "04" aoc202004 (222, 140)
          , assertAoC2020 "05" aoc202005 (880, 731)                             -- Day05
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
          , assertAoC2020 "15" aoc202015 (1259, "MISSING")                      -- Day15 !! part2 is too slow, it was implemented in Go
          , assertAoC2020 "16_invalid" (fst . aoc202016) 71                     -- Day16
          , assertAoC2020 "16" aoc202016 (23044, 3765150732757)
          , assertAoC2020 "17_sample" aoc202017 (112, 848)                      -- Day17
          , assertAoC2020 "17" aoc202017 (257, 2532)
          , assertAoC2020 "18" aoc202018 (86311597203806, 276894767062189)      -- Day18
          , assertAoC2020 "19_sample1" aoc202019 "MISSING"                      -- Day19 !! part2 is missing
          , assertAoC2020 "19_sample2" aoc202019 "MISSING"
          , assertAoC2020 "19" aoc202019 "MISSING"
          , assertAoC2020 "20_sample" aoc202020 (20899048083289, "MISSING")     -- Day20 !! part2 is missing
          , assertAoC2020 "20" aoc202020 (104831106565027, "MISSING")
          , assertAoC2020 "21_sample" aoc202021 (5, "mxmxvkd,sqjhc,fvjkl")      -- Day21
          , assertAoC2020 "21" aoc202021 (2485, "bqkndvb,zmb,bmrmhm,snhrpv,vflms,bqtvr,qzkjrtl,rkkrx")
          , assertAoC2020 "22_sample" aoc202022 (306, 291)                      -- Day22
          , assertAoC2020 "22" aoc202022 (31957, 33212)
          , assertAoC2020 "23_sample" aoc202023 ("67384529", "149245887792")    -- Day23
          , assertAoC2020 "23" aoc202023 ("29385746", "680435423892")
          , assertAoC2020 "24_sample" aoc202024 (10, 2208)                      -- Day24
          , assertAoC2020 "24" aoc202024 (232, 3519)
          , assertAoC2020 "25_sample" aoc202025 14897079                        -- Day25
          , assertAoC2020 "25" aoc202025 12285001
          ]
