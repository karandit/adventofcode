module AoC2022.Day23
( aoc202223
) where

import Data.List (foldl', scanl', find, intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import Utils (readInt, applyN, freq, (|>))

dN =  ( 0, -1)
dNW = (-1, -1)
dS  = ( 0,  1)
dW  = (-1,  0)
dE  = ( 1,  0)
dSW = (-1,  1)
dNE = ( 1, -1)
dSE = ( 1,  1)

quadrants =
  [ (dN, [dN, dNE, dNW])
  , (dS, [dS, dSE, dSW])
  , (dW, [dW, dNW, dSW])
  , (dE, [dE, dNE, dSE])
  ]

adjacents = [ dNW, dW, dSW, dS, dSE, dE, dNE, dN]

addXY (x, y) (dx, dy) = (x + dx, y + dy)

rotate (x:xs) = xs ++ [x]

aoc202223 input = (part1, part2) where
  elves = S.fromList [(x,y)| (y, row) <- zip [0..] $ lines $ input, (x, v) <- zip [0..] row, v /= '.']

  step acc@(m, quads) = let 
          m1 = m |> S.filter (\pos -> adjacents |> map (addXY pos) |> any (\p -> S.member p m))  

          m2 = m1 |> S.toList |> map (\p -> (p, "")) |> M.fromList
                  |> M.mapWithKey (\pos _ -> quads
                                  |> find (\(dir, qs) -> qs
                                                  |> map (addXY pos)
                                                  |> all (\p -> S.notMember p m))
                                  |> fmap (\(dir, qs) -> addXY pos dir))
                  |> M.filter Maybe.isJust
                  |> M.map (Maybe.fromMaybe (-100,-100))
          freqs = m2 |> M.elems |> freq 
          m3 = m2 |> M.filterWithKey (\pos v -> case M.lookup v freqs of
                                                  Nothing -> error "Pos not found in freqs"
                                                  Just times -> times == 1)
          s2 = m |> S.map (\pos -> case M.lookup pos m3 of
                                          Nothing -> pos
                                          Just proposed -> proposed) 
          in (s2, rotate quads)

  elves10 = applyN 10 step (elves, quadrants) |> fst |> S.toList

  (minX, maxX) = elves10 |> map fst |> (\is -> (minimum is, maximum is))
  (minY, maxY) = elves10 |> map snd |> (\is -> (minimum is, maximum is))

  part1 = (maxX - minX + 1) * (maxY - minY + 1) - S.size elves
  (part2,_,_) = until (\(i, (es, qs), (esN, qsN)) -> es == esN) 
               (\(i, acc, accN) -> (i+1, accN, step accN))
               (1, (elves, quadrants), step (elves, quadrants))   
