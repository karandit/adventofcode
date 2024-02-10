module AoC2023.Day05
( aoc202305
) where

import Utils (readInt, chunksOf, (|>))
import Data.List (partition, sortOn)
import Data.List.Utils (split)
import Data.Foldable (foldl', foldl1, find)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

parseMap (title:nrs) = let
  [from, to] = title |> words |> head |> split "-to-"
  mappings = [(dst, src, lng) | [dst, src, lng] <- map (map readInt . words) nrs]
  in (from, (to, sortOn (\(_, src, _) -> src) mappings))

process maps seeds = let  
  (_, ns) = until
        (\(found, _) -> found == Nothing)
        (\(Just (to, mappings), ns)  -> (M.lookup to maps, convert mappings ns)) 
        (M.lookup "seed" maps, seeds)
  in ns

convert :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
convert mappings seeds = concatMap (convert_ mappings) seeds where
 
  convert_ :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
  convert_ mappings (from, fromLn) = 
      
      case (find (\(dst, src, lng) -> src <= from && from < src + lng ) mappings) of
        Just (dst, src, lng) -> if from + fromLn - 1 <= src + lng - 1
                   then [(dst + from - src, fromLn)]
                   else (dst + from - src, src + lng - from): (convert_ mappings (src + lng, fromLn - (src + lng - from) )) 
        Nothing ->
              let (_, biggers) = mappings |> partition (\(dst, src, lng) -> src < from)
              in if length biggers == 0
                 then [(from, fromLn)]
                 else let (dst1, src1, lng1) = head biggers
                      in if from + fromLn - 1 < src1
                         then [(from, fromLn)]
                         else (from, src1 - from): (convert_ mappings (src1, fromLn-(src1-from)))

aoc202305 input = (part1, part2) where
  inputs = input|> lines |> split [""]
  seeds = inputs |> head |> head |> words |> tail |> map readInt
  maps  = inputs |> tail |> map parseMap |> M.fromList

  solve rs = minimum [from | (from, _) <- process maps rs]

  part1 = solve [(a, 1) | a <- seeds]
  part2 = solve [(a, b) | [a, b] <- chunksOf 2 seeds] 
