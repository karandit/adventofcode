{-# Language NumericUnderscores #-}
module AoC2023.Day14
( aoc202314
) where

import Utils ((|>))
import Data.List (transpose, intercalate)
import Data.List.Utils (split)
import qualified Data.Map as M

rotL = transpose . map reverse
rotR = map reverse . transpose

tilt s = s
     |> split "#"
     |> map (\sl -> let rounded = length $ filter ((==)'O') sl
                        slLen = length sl 
                    in (replicate rounded 'O') ++ (replicate (slLen - rounded) '.') )
     |> intercalate "#"

calcRocks rocks = rocks
     |> map (\l -> l |> filter ((==) 'O') |> length)
     |> reverse
     |> zip [1..]
     |> map (\(i,l) -> i * l)
     |> sum

nwseTilt rocks = map (reverse .tilt . reverse) $
     rotL $ map tilt $
     rotR $ map tilt $ rotR $
     map tilt $ rotL $
     rocks

findCycle :: Ord a => [a] -> (Int, Int)
findCycle = go M.empty 0
  where
    go _ _ [] = error "no cycle"
    go seen i (x:xs) =
      case M.lookup x seen of
        Nothing -> go (M.insert x i seen) (i + 1) xs
        Just j  -> (j, i)

aoc202314 input = (part1, part2) where
  inputs = input |> lines

  part1 = inputs |> rotL |> map tilt |> rotR |> calcRocks

  infRounds = iterate nwseTilt inputs
  (cStart, cEnd) = findCycle infRounds
  ith = cStart + ((1_000_000_000 - cStart) `mod` (cEnd - cStart))
  part2 = calcRocks (infRounds !! ith)
