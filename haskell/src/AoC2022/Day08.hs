module AoC2022.Day08
 where

import Data.Function (on)
import Utils (readInt, (|>))
import qualified Data.Map as M
import Data.List (sortBy)

main :: IO ()
main = do 
    input <- readFile "../input.txt"
    input <- readFile "../inputs.txt"
    let
        inputs = input |> lines 
        interior  = inputs |> tail 
        part1 = inputs |> zip [0 ..] |> map (\(r, row) -> 
                        row |> zip [0..] |> map(\(c, v) -> 
                            ((r, c), readInt [v])))
                |> concat
                |> M.fromList

        visib m (r0, c0) v0 = let
               toLeft =  m |> M.filterWithKey (\(r, c) v -> r == r0 && c < c0 && v >= v0) |> null
               toRight = m |> M.filterWithKey (\(r, c) v -> r == r0 &&c > c0 && v >= v0) |> null
               toDown = m |> M.filterWithKey (\(r, c) v -> r > r0 && c== c0 && v >= v0) |> null
               toTop = m |> M.filterWithKey (\(r, c) v -> r < r0 &&c == c0 && v >= v0) |> null
            in toLeft || toRight || toDown || toTop 

        taken r n [] = r
        taken r n (x:xs) = if x<n then taken (r+1) n xs else r+1  

        scenic m (r0, c0) v0 = let
               toLeft =  m |> M.filterWithKey (\(r, c) v -> r == r0 && c < c0) |> M.toList |> map (\((r,c), v) -> (c,v)) |> sortBy (compare `on` fst) |> map snd |> reverse |> taken 0 v0
               toRight = m |> M.filterWithKey (\(r, c) v -> r == r0 &&c > c0)|> M.toList |> map (\((r,c), v) -> (c,v)) |> sortBy (compare `on` fst) |> map snd |> taken 0 v0
               toDown = m |> M.filterWithKey (\(r, c) v -> r > r0 && c== c0)|> M.toList |> map (\((r,c), v) -> (r, v)) |> sortBy (compare `on` fst) |> map snd |> taken 0 v0
               toTop = m |> M.filterWithKey (\(r, c) v -> r < r0 &&c == c0)|> M.toList |> map (\((r,c), v) -> (r,v)) |> sortBy (compare `on` fst) |> map snd |> reverse |> taken 0 v0
            --in show v0 ++ ":" ++ show toLeft ++ "*" ++ show toRight ++ "*" ++ show toDown ++ "*" ++ show toTop 
            in toLeft * toRight * toDown * toTop 
        f1 = part1 |> M.mapWithKey (\(r, c) v -> visib part1 (r, c) v)
        s1 = f1 |> M.filter id |> M.size

        f2 = part1 |> M.mapWithKey (\(r, c) v -> scenic part1 (r, c) v)
        s2 = f2 |> M.toList |> map snd |> maximum

    --putStrLn $ show $ f1
    putStrLn $ show $ s1

--    putStrLn $ show $ f2
 --   putStrLn $ show $ s2
