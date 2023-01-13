module AoC2020.Day07
  ( aoc202007,
  )
where

import Data.List.Utils (split)
import qualified Data.Map as M
import Utils (readInt)

parseBags :: M.Map String [(String, Int)] -> String -> M.Map String [(String, Int)]
parseBags store input =
  let topBag : otherBags : _ = split "contain" input
      key1 : key2 : _ = words topBag
      smallBags =
        map (\s -> let sp : nn : v1 : v2 : _ = split " " s in (v1 ++ " " ++ v2, if nn /= "no" then readInt nn else 0)) $
          split "," $ otherBags
      color = key1 ++ " " ++ key2
   in case M.lookup color store of
        Just f -> M.update (\v -> Just (smallBags ++ v)) color store
        Nothing -> M.insert color smallBags store

readBags :: String -> M.Map String [(String, Int)]
readBags = foldl parseBags M.empty . lines

shiny = "shiny gold"

canContain store bagName =
  maybe False (\names -> elem shiny names || any (canContain store) names) $
    M.lookup bagName store

weight bagName store =
  maybe 0 (\innerBags -> 1 + (sum $ map (\(n, w) -> w * weight n store) innerBags)) $
    M.lookup bagName store

part1 input =
  let store = M.map (map fst) $ readBags input
  in length . filter (canContain store) $ M.keys store

{- | Part 2
>>> :{
part2 "shiny gold bags contain 2 dark red bags.\n\
      \dark red bags contain 2 dark orange bags.\n\
      \dark orange bags contain 2 dark yellow bags.\n\
      \dark yellow bags contain 2 dark green bags.\n\
      \dark green bags contain 2 dark blue bags.\n\
      \dark blue bags contain 2 dark violet bags.\n\
      \dark violet bags contain no other bags."
:}
126
-}
part2 input = (+) (-1) . weight shiny . readBags $ input

aoc202007 input = (part1 input, part2 input)
