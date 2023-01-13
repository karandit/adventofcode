module AoC2020.Day16
  ( aoc202016,
  )
where

import Data.List (delete, find)
import Data.List.Utils (split)
import Data.String.Utils (startswith)
import Utils (readInt)

type Rule = (String, [(Int, Int)])

type Ticket = [Int]

readInput :: String -> ([Rule], Ticket, [Ticket])
readInput input =
  let rules : your : nearBy : [] = split [""] $ lines input
   in (map readRule rules, readYour your, readNearBy nearBy)

headTuple2 :: [a] -> (a, a)
headTuple2 (a1 : a2 : []) = (a1, a2)

--departure location: 36-363 or 377-962
readRule :: String -> Rule
readRule rule =
  let fieldName : numsByOr : [] = split ": " rule
      nums = map (headTuple2 . map readInt . split "-") . split " or " $ numsByOr
   in (fieldName, nums)

--your ticket:
--89,179,173,167,157,127,163,113
readYour :: [String] -> Ticket
readYour = map readInt . split "," . last

--nearby tickets:
--930,274,273,471,282,613,191,559,820
readNearBy :: [String] -> [Ticket]
readNearBy = map (map readInt . split ",") . tail

{- | Part 1
>>> :{
aoc202016a "class: 1-3 or 5-7\n\
           \row: 6-11 or 33-44\n\
           \seat: 13-40 or 45-50\n\
           \\n\
           \your ticket:\n\
           \7,1,14\n\
           \\n\
           \nearby tickets:\n\
           \7,3,47\n\
           \40,4,50\n\
           \55,2,20\n\
           \38,6,12"
:}
71
-}
aoc202016a input =
  let (rules, _, nearByTickets) = readInput input
      allRules = concat $ map snd rules
   in sum $ concat $ map (\nrs -> filter (\n -> not $ any (\(b, e) -> n >= b && n <= e) allRules) nrs) nearByTickets

validateMapping mapping ticket =
  let idxs = zip [0 ..] ticket
      ruleOk ((a1, b1) : (a2, b2) : []) n = (n >= a1 && n <= b1) || (n >= a2 && n <= b2)
      checkRule (_, rule) candidates (idx, n) =
        if ruleOk rule n
          then candidates
          else delete idx candidates
   in map (\(rule, cand) -> (rule, foldl (checkRule rule) cand idxs)) mapping

shrinkMappings mappings = shrinkMappings' mappings []

shrinkMappings' mappings res =
  let foundSingle = find (\(rule, cands) -> length cands == 1) mappings
   in case foundSingle of
        Nothing -> res
        Just foundRule@(_, [n]) ->
          shrinkMappings' (map (\(rule, ns) -> (rule, delete n ns)) mappings) (foundRule : res)

aoc202016b input =
  let (rules, myticket, nearByTickets) = readInput input
      allRules = concat $ map snd rules
      validTickets =
        filter
          ( \nrs ->
              all (\n -> any (\(b, e) -> n >= b && n <= e) allRules) nrs
          )
          nearByTickets
      allCands = [0 .. (length rules) -1]
      mappings = map (\rule -> (rule, allCands)) rules
      foundMappings = foldl validateMapping mappings $ validTickets
      solution = filter (\((ruleName, _), _) -> startswith "departure" ruleName) $ shrinkMappings foundMappings
   in product . map ((!!) myticket . head . snd) $ solution

aoc202016 input = (aoc202016a input, aoc202016b input)
