module AoC2021.Day12
( aoc202112
) where

import Data.List (foldl', nub)
import Data.List.Utils (split)
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Utils (readInt, (|>))

main :: IO ()
main = do 
    input <- readFile "inputs/2021/day12_small.txt"
    input <- readFile "inputs/2021/day12_large1.txt"
    input <- readFile "inputs/2021/day12.txt"
    input <- readFile "inputs/2021/day12_large2.txt"
    --putStrLn $ unlines $ map show $ aoc202112 input
    putStrLn $ show $ aoc202112 input

aoc202112 input = part3 |> nub |> length where

    inputs = input |> lines |> map parseCave
    parseCave s = s |> split "-" |> \[a, b] -> (a, b)  
    edges = inputs |> foldl' (\acc (a,b) -> acc 
                                |> (if a == "end" || b == "start" then id else M.insertWith (++) a [b])
                                |> (if b == "end" || a == "start" then id else M.insertWith (++) b [a])) M.empty

    explore1 visited "end" = 1
    explore1 visited srcCave
       | (srcCave |> head |> Char.isLower) && (M.findWithDefault 0 srcCave visited |> (==) 1) = 0
       | otherwise =  M.lookup srcCave edges |> Maybe.maybe 0 ( sum . map (explore1 (MS.insertWith (+) srcCave 1 visited)) ) 
    part1 = explore1 M.empty "start"
    
    -- explore1 visited path "end" = [path]
    -- explore1 visited path srcCave = 
    --    if (srcCave |> head |> Char.isLower) && (1 == (M.lookup srcCave visited |> Maybe.fromMaybe 0))
    --    then []
    --    else case M.lookup srcCave edges of
    --         Nothing -> []
    --         Just dstCaves -> dstCaves
    --                  |> map (\dstCave -> explore1 (M.insertWith (\oV nV -> oV + 1) srcCave 1 visited) (path ++ " " ++ srcCave) dstCave)
    --                  |> concat
    -- part1 = explore1 M.empty "" "start" |> length

    explore :: String -> M.Map String Int -> [String] -> String -> [[String]]
    explore selectedCave visited path "end" =  [path |> reverse]
    explore selectedCave visited path srcCave = 
       if (srcCave |> head |> Char.isUpper)
       then MS.lookup srcCave edges |> Maybe.maybe [] (concat . map (explore selectedCave visited (srcCave:path)))
       else if (1 + (if (selectedCave == srcCave && "start" /= srcCave) then 1 else 0) == (M.lookup srcCave visited |> Maybe.fromMaybe 0))
            then []
            else MS.lookup srcCave edges |> Maybe.maybe [] (concat .  map (\dstCave -> 
                                if selectedCave == "" 
                                -- then (if srcCave == "start" then 0 else (explore srcCave (M.insert srcCave 1 visited) dstCave))  + (explore "" (M.insert srcCave 0 visited) dstCave)
                                then (   (explore srcCave (MS.insertWith (+) srcCave 1 visited) (srcCave:path) dstCave)
                                      ++ (explore ""      (MS.insertWith (+) srcCave 1 visited) (srcCave:path) dstCave)
                                     )
                                else explore selectedCave (MS.insertWith (+) srcCave 1 visited) (srcCave:path) dstCave
                            ))

    part2 = explore "" MS.empty [] "start" --  > nub |> length

    explore3 :: M.Map String Int -> M.Map String Int -> [String] -> String -> [[String]]
    explore3 limits visited path "end" =  [path] -- `seq` []  -- |>debug "found"
    explore3 limits visited path srcCave
        | srcCave |> head |> Char.isUpper = M.lookup srcCave edges |> Maybe.maybe [] (concat . map (explore3 limits visited (srcCave:path) ))
        | (M.lookup srcCave limits |> Maybe.fromMaybe 0)  == (M.lookup srcCave visited |> Maybe.fromMaybe 0) = []
        | otherwise = M.lookup srcCave edges |> Maybe.maybe [] (concat . map (explore3 limits (M.insertWith (+) srcCave 1 visited) (srcCave:path) ) )

    limitsInit = edges |> M.filterWithKey (\caveName _ -> Char.isLower $ head $ caveName) |> M.map (const 1)
    -- limitsInit' = M.insert "b" 2 limitsInit
    part3 = edges |> M.keys 
                  |> filter (\caveName -> (Char.isLower $ head $ caveName) && caveName /= "start" && caveName /= "end")
                  |> map (\caveName -> explore3 (M.insert caveName 2 limitsInit) M.empty [] "start")
                  |> concat

