module AoC2022.Day07
  ( aoc202207,
  )
where

import Data.List (intercalate, isSuffixOf)
import Data.List.Utils (split)
import Data.Map as M (adjust, empty, filterWithKey, insertLookupWithKey, toList)
import Utils (readInt, (|>))

aoc202207 input = (part1, part2)
  where
    calcSize pwd fs = fs |> M.filterWithKey (\k _ -> isSuffixOf pwd k) |> M.toList |> map snd |> concat |> sum

    parse (pwd, fs) cmd = case cmd of
      "$ cd .." -> (tail pwd, fs)
      "$ ls" -> (pwd, fs)
      'd' : 'i' : 'r' : ' ' : _ -> (pwd, fs)
      '$' : ' ' : 'c' : 'd' : ' ' : dir ->
        let newPwd = dir : pwd
            newFs = fs |> M.insertLookupWithKey (\_ nv ov -> ov) (intercalate "/" newPwd) [] |> snd
         in (newPwd, newFs)
      fileDescr ->
        let newSize = fileDescr |> split " " |> head |> readInt
            newFs = fs |> M.adjust (\sizes -> newSize : sizes) (intercalate "/" pwd)
         in (pwd, newFs)

    fileSystem = input |> lines |> foldl parse ([], M.empty) |> snd
    dirSizes = fileSystem |> M.toList |> map (\(dir, _) -> calcSize dir fileSystem)
    rootSize = calcSize "/" fileSystem
    minSize = rootSize - 40000000

    part1 = dirSizes |> filter (<= 100000) |> sum
    part2 = dirSizes |> filter (>= minSize) |> minimum
