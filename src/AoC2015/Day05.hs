module AoC2015.Day05
( aoc201505
) where

import Data.List (intersect, isInfixOf)
import Utils (readInt, (|>))

aoc201505 input = (part1, part2) where
    inputs = input |> lines
    isNice1 s = (intersect s "aeiou" |> length |> \l -> l > 2)
               && (zip (init s) (tail s) |> any (\(x1,x2) -> x1 == x2))
               && (["ab", "cd","pq", "xy"] |> all (\x -> isInfixOf x s |> not))
    part1 = inputs |> filter isNice1 |> length

    isThree (b:c:[]) = False
    isThree (a:b:c:x) = if a == c then True else isThree (b:c:x)

    isTwoConsec (a:b:[]) = False
    isTwoConsec (a:b:x) = if isInfixOf [a,b] x then True else isTwoConsec (b:x)

    isNice2 s = isThree s && isTwoConsec s
    part2 = inputs |> filter isNice2 |> length
