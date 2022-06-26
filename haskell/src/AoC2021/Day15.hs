{-# LANGUAGE LambdaCase #-}
module AoC2021.Day15
( aoc202115
) where

import Data.List (sortBy, groupBy, minimumBy, foldl')
import qualified Data.Char as Char (chr, ord)
import qualified Data.Maybe as Maybe (maybe, isJust)
import qualified Data.Map.Strict as M (Map, fromList, toAscList, toList, singleton, delete, lookup, alter)
import qualified Data.Set as S (Set, empty, insert, member, notMember)
import Utils ((|>))

printMatrix m = m |> M.toAscList 
               |> sortBy  (\((x1,y1),_) ((x2,y2),_) -> compare (y1, x1) (y2, x2))
               |> groupBy (\((x1,y1),_) ((x2,y2),_) -> y1 == y2)
               |> map (\row -> row |> map (Char.chr . (+) 48 . snd))
               |> unlines

movePair (x, y) (dx, dy) = (x + dx, y + dy)

type Coord = (Int, Int)

main:: IO ()
main= do
    input <- readFile "../inputs/2021/day15_sample.txt"
    input <- readFile "../inputs/2021/day15.txt"
    putStrLn $ "       input        :"
    --putStrLn $ printMatrix $ risks1
    putStrLn $ "       part1        :" ++ (show $ aoc202115 input)
    --putStrLn $ printMatrix $ risks2
    --putStrLn $ show $ M.size risks2

aoc202115 input = (part1, part2) where
        inputs = input |> lines
             |> zip [0..]
             |> map (\(y,row) -> zip [0..] row
                    |> map (\(x,c) -> ((x,y), Char.ord c - 48))
                    )

        lenX = inputs |> head |> length
        lenY = inputs |> length
        compareDist (_, dista) (_, distb) = compare dista distb

        dijkstra :: (Coord, M.Map Coord Int, M.Map Coord Int, S.Set Coord, Maybe Int) -> (Coord, M.Map Coord Int, M.Map Coord Int, S.Set Coord, Maybe Int)
        dijkstra (target, risks, unvisited, visited, _) =
                let 
                (xyu, distu) = unvisited |> M.toList |> minimumBy compareDist
                visited' = S.insert xyu visited
                unvisited' = M.delete xyu unvisited
                unvisited'' = [(0, 1), (0, -1), (-1, 0), (1,0)] 
                         |> map (movePair xyu)
                         |> filter (\xyadj -> S.notMember xyadj visited )
                         |> foldl' (\acc xyadj -> risks |> M.lookup xyadj |> Maybe.maybe acc 
                                        (\riskadj -> acc |> M.alter 
                                            (\case
                                                 Nothing      -> Just (distu+riskadj)
                                                 Just distOld -> Just (min distOld (distu+riskadj))
                                            ) xyadj
                                        )
                                    ) unvisited'
                in (target, risks, unvisited'', visited', if xyu == target then Just distu else Nothing)

        risks1 = inputs |> concat |> M.fromList
        (_, _, _, _, Just part1) = until (\(_, _, _, _, sol) -> sol |> Maybe.isJust)
                                    dijkstra ((lenX-1, lenY-1), risks1, M.singleton (0, 0) 0, S.empty, Nothing)

        over9 i = if i > 9 then i-9 else i
        inputs2 = input |> lines
             |> zip [0..]
             |> map (\(y, row) -> zip [0..] row
                    |> map (\(x,c) -> [( (x+dx*lenX,y+dy*lenY), over9 ((Char.ord c - 48)+dx+dy) ) | dx <-[0..4], dy<-[0..4]]) |> concat
                    )
        risks2 = inputs2 |> concat |> M.fromList
        (_, _, _, _, Just part2) = until (\(_, _, _, _, sol) -> sol |> Maybe.isJust)
                                    dijkstra ((lenX*5-1, lenY*5-1), risks2, M.singleton (0, 0) 0, S.empty, Nothing)
