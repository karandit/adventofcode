module AoC2023.Day08
( aoc202308
) where

import Utils (readInt, (|>))
import Data.Char (isDigit)
import Data.List.Utils (replace, split)
import Data.Foldable (foldl')
import qualified Data.Map as M

nextNode 'R' (l, r) = r 
nextNode 'L' (l, r) = l 

main :: IO ()
main = do 
  input <- readFile "../inputs/2023/day08M.txt"
  --input <- readFile "../inputs/2023/day08S.txt"
  --input <- readFile "../inputs/2023/day08L.txt"
  --input <- readFile "../inputs/2023/day08.txt"
  let inputs = input |> lines
      instrs = inputs |> head
      network = inputs |> tail |> tail
             |> map (\s -> s
                    |> replace "= " ""
                    |> replace "(" ""
                    |> replace ")" ""
                    |>replace "," ""
                    |> words
                    |> (\[a, b, c] -> (a, (b, c))))
             |> M.fromList

      (finalNode, part1, _) = until
        (\(node, _, _) -> node == "ZZZ")
        (\(node, steps, i:instrs)  -> (nextNode i (network M.! node), steps + 1, instrs)) 
        ("AAA", 0, cycle instrs)

      getCycle start cond = let
          (finalNode, res, _) = until
            (\(node, _, _) -> cond node)
            (\(node, steps, i:instrs)  -> (nextNode i (network M.! node), steps + 1, instrs)) 
            (start, 0, cycle instrs)
          in res
    
      startNodes = network |> M.keys |> filter (\name -> last name == 'A')
      -- (finalNodes, part2, _) = until
      --   (\(nodes, _, _) -> all (\name -> last name == 'Z' ) nodes)
      --   (\(nodes, steps, i:instrs)  -> (map (\node -> nextNode i (network M.! node)) nodes, steps + 1, instrs)) 
      --   (startNodes, 0, cycle instrs)
      cycls = map (\startNode -> getCycle startNode (\n -> last n == 'Z')) startNodes
  putStrLn $ "instrs: " ++ instrs
  putStrLn $ unlines $ map show $ M.toList $ network
  --putStrLn $ finalNode
  putStrLn $ show part1
  putStrLn $ show startNodes
  --putStrLn $ show finalNodes
  putStrLn $ show cycls
  --putStrLn $ show $ aoc202308 input

aoc202308 input = ("MISSING", "MISSING") where
