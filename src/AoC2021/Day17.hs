module AoC2021.Day17
( aoc202117
) where

import qualified Data.Maybe as Maybe
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import Data.List (foldl')
import Data.List.Utils (split)
import Utils (readInt, (|>))

main :: IO ()
main = do 
    input <- readFile "inputs/2021/day17_sample.txt"
    --input <- readFile "inputs/2021/day17.txt"
    putStrLn $ show $ aoc202117 input

aoc202117 input = (part1, part2) where
    [[xlower, xupper], [ylower, yupper]]  = input 
                    |> drop 13 |> split ", "
                    |> map (\s -> s |> split "=" |> tail |> head |> split ".." |> map readInt)  

    ylowerp = abs ylower
    part1 = ylowerp * (ylowerp - 1) `div` 2

    yvels = [ylower..ylowerp-1] 
            |> map (\y0 -> (y0, scanl (+) y0 [y0-1, y0-2..] 
                                |> takeWhile (\y -> y >= ylower) 
                                |> zip [1..] 
                                |> filter (\(i,y) -> yupper >= y && y >= ylower)
                            ))
    yrangeM = yvels |> foldl' (\acc (y0, yrange) -> yrange 
                                    |> foldl' (\acc' (i, _)-> acc' |> MS.insertWith (++) i [y0]) acc
                              ) M.empty
    iMax = M.findMax yrangeM |> fst
    sol = [2..xlower-1]
            |> map (\x0 -> (x0, 
                scanl (\acc v -> acc + max 0 v) x0 [x0-1,x0-2..] 
                |> zip [1..iMax] 
                |> filter (\(i,x) -> xlower <= x && x <= xupper)))
            |> map (\(x0, xranges) -> (x0, 
                    xranges
                    |> foldl' (\acc (i,_) -> M.lookup i yrangeM |> Maybe.fromMaybe [] |> S.fromList |> S.union acc) S.empty
                    |> S.toList
                    |> length)
                    )
    part2 = (abs (xupper - xlower) +1)*(abs (yupper-ylower)+1) + (sol |> map snd |> sum)
