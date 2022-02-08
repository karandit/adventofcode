module AoC2021.Day10
( aoc202110
) where

import Data.List (foldl', sort)
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

aoc202110 input = (part1, part2) where
    inputs = input |> lines

    isOpening c = c == '[' || c == '{' || c == '<' || c == '('
    isMatching '{' '}' = True
    isMatching '[' ']' = True
    isMatching '(' ')' = True
    isMatching '<' '>' = True
    isMatching  _   _  = False

    parseChunk s = foldl' (\acc@(stack, res) c -> case res of
                        Nothing -> if isOpening c
                                   then (c:stack, res)
                                   else if isMatching (head stack) c
                                        then (tail stack, res)
                                        else (stack, Just c)
                        Just _ -> acc) ([], Nothing) s
    chunks = inputs |> map parseChunk

    cost ')' = 3
    cost ']' = 57
    cost '}' = 1197
    cost '>' = 25137
    part1 = chunks |> map (\(_, chunk) -> maybe 0 cost chunk) |> sum

    autocom '(' = 1
    autocom '[' = 2
    autocom '{' = 3
    autocom '<' = 4
    part2 = chunks |> filter (\(_, corrupt) -> Maybe.isNothing corrupt)
                   |> map (\(chunk, _) -> foldl' (\acc c -> acc * 5 + autocom c) 0 chunk)
                   |> (\scores -> let sortedScores = sort scores in sortedScores !! (length sortedScores `div` 2))
