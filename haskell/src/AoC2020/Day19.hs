module AoC2020.Day19
  ( aoc202019,
  )
where

import qualified Data.List.Utils as LL
import qualified Data.Map as M
import qualified Data.Set as S
import Utils

data MsgRule = Final Char | Subs [[Int]] deriving (Show)

parseRule s =
  let id : rest : [] = LL.split ": " s
   in (readInt id, readComps rest)

readComps ['"', c, '"'] = Final c
readComps s = Subs $ map (map readInt . LL.split " ") $ LL.split " | " s

combis :: [[a]] -> [[a]]
combis subs = foldl (\acc cl -> concat $ map (\acc1 -> map (\cl1 -> acc1 ++ [cl1]) cl) acc) [[]] subs

dehyd :: M.Map Int MsgRule -> [[Char]]
dehyd store =
  let dehyd' k = case M.lookup k store of
        --Nothing -> [""]
        Just found@(Final c) -> [[c]]
        Just (Subs subs) ->
          let ks = show k
              resubs0 = map (\sub -> map dehyd' sub) $ subs
              combis0 = map (\sub -> combis sub) $ resubs0
              joined0 = map (\sub -> map (LL.join []) sub) $ combis0
              concat0 = concat $ joined0
              pri =
                debug
                  ( "k:" ++ ks
                      ++ " resubs0:"
                      ++ (show resubs0)
                      ++ " combis0:"
                      ++ (show $ combis0)
                      ++ " joined0:"
                      ++ (show $ joined0)
                      ++ " result:"
                  )
                  $ concat0
           in concat0
   in dehyd' 0

main :: IO ()
main = do
  input <- readFile "inputs/2020/day19_sample1.txt"
  let srules : msgs : [] = LL.split [""] $ lines input
      rules0 = M.fromList $ map parseRule srules
      rules = rules0
      --rules = M.insert 8 (Subs [[42], [42, 42]]) rules0
      hyds = dehyd rules
      hydSet = S.fromList hyds
      res = length $ filter (\msg -> S.member msg hydSet) msgs
  putStrLn $ "       rules        :"
  putStrLn $ unlines $ map show $ M.toList rules
  putStrLn $ "       hyds        :"
  putStrLn $ show $ length $ hyds
  putStrLn $ show $ length $ hydSet

--putStrLn $ "       msgs        :"
--putStrLn $ unlines $ msgs
--putStrLn $ show res

aoc202019 input = "MISSING"
