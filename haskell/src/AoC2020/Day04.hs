module AoC2020.Day04
  ( aoc202004,
  )
where

import qualified Data.Char as Char
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Data.String.Utils as SS
import Utils
import Data.List.Utils (split)

parseRow input =
  let parseTokens word =
        let a : b : xs = words $ replace ':' ' ' word
         in (a, b)
   in M.fromList $ map parseTokens $ words input

byr f = let b = readInt f in b >= 1920 && b <= 2002

iyr f = let b = readInt f in b >= 2010 && b <= 2020

eyr f = let b = readInt f in b >= 2020 && b <= 2030

hcl f = (7 == length f) && (head f == '#') && (all Char.isHexDigit $ tail f)

ecl f = S.member f $ S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pid f = (9 == length f) && (all Char.isDigit f)

hgt f =
  (SS.endswith "cm" f && (let s = readInt $ SS.replace "cm" "" f in s >= 150 && s <= 193))
    || (SS.endswith "in" f && (let s = readInt $ SS.replace "in" "" f in s >= 59 && s <= 76))

fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

checks = [byr, iyr, eyr, hgt, hcl, ecl, pid]

fieldsPresent store = all (\f -> M.member f store) fields

fieldsPresentAndValid store =
  all
    ( \(field, check) ->
        (M.member field store)
          && (maybe False check $ M.lookup field store)
    )
    $ zip fields checks

solve pred input = input
            |> lines
            |> split [""]
            |> map (foldl (\acc row -> M.union acc $ parseRow row) M.empty)
            |> filter pred
            |> length

aoc202004 input = (part1, part2)
  where
    part1 = solve fieldsPresent input
    part2 = solve fieldsPresentAndValid input
