module AoC2021.Day12
( aoc202112
) where

import Data.List (foldl')
import Data.List.Utils (split)
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as M
import Utils ((|>))

type Graph = M.Map String [String]
type Visited = M.Map String Int

aoc202112 input = (part1, part2) where

    parseCave s = s |> split "-" |> \[a, b] -> (a, b)  
    inputs = input |> lines |> map parseCave
    graph = inputs |> foldl' (\acc (a,b) -> acc
                                |> (if a == "end" || b == "start" then id else M.insertWith (++) a [b])
                                |> (if b == "end" || a == "start" then id else M.insertWith (++) b [a])) M.empty

    isBigCave :: String -> Bool
    isBigCave caveName = caveName |> head |> Char.isUpper

    notVisited :: Visited -> String -> Bool
    notVisited visited caveName = M.notMember caveName visited
    
    noTwice :: Visited -> Bool
    noTwice visited = M.null $ M.filter (\count -> count > 1) visited

    oneSmallAllowedTwice :: Visited -> String -> Bool
    oneSmallAllowedTwice visited caveName = notVisited visited caveName || noTwice visited

    explorePaths :: String -> Visited -> (Visited -> String -> Bool) -> Graph -> [Int]
    explorePaths "end"   visited _    _     = [1]
    explorePaths curCave visited pred graph =
        let visited' = if isBigCave curCave then visited else M.insertWith (+) curCave 1 visited
        in foldMap (\h -> explorePaths h visited' pred graph) . filter (pred visited') . Maybe.fromMaybe [] . M.lookup curCave $ graph

    solve :: (Visited -> String -> Bool) -> Graph -> Int
    solve pr gr = explorePaths "start" M.empty pr gr |> length

    part1 = solve notVisited graph
    part2 = solve oneSmallAllowedTwice graph
