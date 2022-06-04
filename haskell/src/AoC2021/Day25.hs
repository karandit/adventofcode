module AoC2021.Day25
  ( aoc202125,
  )
where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

aoc202125 input = part1
  where
    inputs =
      input |> lines
        |> zip [0 ..]
        |> map (\(y, row) -> zip [0 ..] row |> map (\(x, c) -> ((x, y), c)))
        |> concat
    cucumbers0 = inputs |> M.fromList
    maxX = 1 + (inputs |> map fst |> map fst |> maximum)
    maxY = 1 + (inputs |> map fst |> map snd |> maximum)
    isFreePos (x, y) m = M.lookup (x, y) m |> Maybe.maybe True (\v -> v == '.')
    isFreePosV (x, y) m = M.lookup (x, y) m |> Maybe.maybe True (\v -> v /= 'v')
    stepCucumbers cucumbers =
      let (cntMovesEast, cucumbers') =
            cucumbers
              |> M.foldrWithKey
                ( \(x, y) c acc@(cnt, accM) ->
                    let newPos = ((x + 1) `mod` maxX, y)
                     in if c == '>'
                          then
                            isFreePos newPos cucumbers
                              |> (\isFree -> if isFree then (cnt + 1, M.insert newPos c accM) else (cnt, M.insert (x, y) c accM))
                          else acc
                )
                (0, M.empty)
          (cntMovesSouth, cucumbers'') =
            cucumbers
              |> M.foldrWithKey
                ( \(x, y) c acc@(cnt, accM) ->
                    let newPos = (x, (y + 1) `mod` maxY)
                     in if c == 'v'
                          then
                            isFreePosV newPos cucumbers && isFreePos newPos cucumbers'
                              |> (\isFree -> if isFree then (cnt + 1, M.insert newPos c accM) else (cnt, M.insert (x, y) c accM))
                          else acc
                )
                (0, cucumbers')
       in (cntMovesEast + cntMovesSouth, cucumbers'')
    (part1, (lastMoves, _)) = until (\(_, (cnt, _)) -> cnt == 0) (\(idx, (_, cmbrs)) -> (idx + 1, stepCucumbers cmbrs)) (0, (1, cucumbers0))
