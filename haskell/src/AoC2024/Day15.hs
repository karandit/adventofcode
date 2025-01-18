module AoC2024.Day15
( aoc202415
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Utils (split)
import Data.List (foldl')
import Utils (add2d)

type Pos = (Int, Int)
type Grid = M.Map Pos Char
type Pusher = (Pos, Grid) -> Char -> Maybe (S.Set Pos)

aoc202415 input = (part1, part2) where
  [warehouse, movesPerLine] =  split [""] $ lines $input 
  moves = concat movesPerLine
  grid0 = M.fromList [((y, x), v) | (y, row) <- zip [0..] warehouse, (x, v) <- zip [0..] row, v /= '.'] 

  step '^' = (- 1,   0)
  step 'v' = (  1,   0)
  step '>' = (  0,   1)
  step '<' = (  0, - 1)

  ------------------ PART1 ----------
  startPos1 = head $ M.keys $ M.filter (=='@') grid0
  grid1 = M.delete startPos1 grid0

  pusher1 :: Pusher
  pusher1 (robot, grid) move =
    let expandPos _ (y, x) row = S.insert (y, x) row
    in pushBoxes expandPos (robot, grid) move

  ------------------ PART2 ----------
  startPos2 = let (y, x) = startPos1 in (y, 2 * x)

  expandTile '#' = "##"
  expandTile 'O' = "[]"
  grid2 = M.fromList $ concat [ [((y, x * 2), l), ((y, x * 2 + 1), r)] |((y,x), v) <- M.toList grid1, let [l, r] = expandTile v]

  pusher2 :: Pusher
  pusher2 (robot, grid) '>' = pusher1 (robot, grid) '>'
  pusher2 (robot, grid) '<' = pusher1 (robot, grid) '<'
  pusher2 (robot, grid) move =
    let
     expandPos '[' (y, x) row = S.insert (y, x + 1) $ S.insert (y, x) row
     expandPos ']' (y, x) row = S.insert (y, x - 1) $ S.insert (y, x) row
    in pushBoxes expandPos (robot, grid) move

  ----------------------- COMMON --------------
  pushBoxes :: (Char -> Pos -> S.Set Pos -> S.Set Pos) -> (Pos, Grid) -> Char -> Maybe (S.Set Pos)
  pushBoxes expander (robot, grid) move =
    let delta = step move
        checkBox :: Maybe (S.Set Pos) -> Pos -> Maybe (S.Set Pos)
        checkBox Nothing _ = Nothing
        checkBox acc@(Just boxesToPush) pos = case (M.findWithDefault '.' pos grid) of
          '.' -> acc
          '#' -> Nothing
          c -> Just (expander c pos boxesToPush)

        checkBoxes :: Maybe (S.Set Pos) -> S.Set Pos -> Maybe (S.Set Pos)
        checkBoxes Nothing _ = Nothing
        checkBoxes (Just boxesToPush) row =
          case (S.foldl' checkBox (Just S.empty) $ S.map (add2d delta) row) of
          Nothing -> Nothing
          Just nextRow -> if S.null nextRow
                         then Just boxesToPush
                         else checkBoxes (Just (S.union nextRow boxesToPush)) nextRow
    in checkBoxes (Just S.empty) (S.singleton robot)

  nextRound :: Pusher -> (Pos, Grid) -> Char -> (Pos, Grid)
  nextRound pusher (robot, grid) move = case (pusher (robot, grid) move) of
    Nothing -> (robot, grid)
    Just boxesToPush ->
      let delta = step move
          cleanedGrid = foldl' (\acc p -> M.delete p acc) grid boxesToPush
          grid' = foldl' (\acc p -> let Just box = M.lookup p grid in M.insert (add2d p delta) box acc) cleanedGrid boxesToPush
      in (add2d robot delta, grid')

  solve :: Char -> Pusher -> Pos -> Grid -> [Char] -> Int
  solve boxChar pusher robot grid moves =
    let (_, finalGrid) = foldl' (nextRound pusher) (robot, grid) moves
    in sum [100 * y + x | (y, x) <- M.keys $ M.filter (== boxChar) finalGrid]

  part1 = solve 'O' pusher1 startPos1 grid1 moves
  part2 = solve '[' pusher2 startPos2 grid2 moves
