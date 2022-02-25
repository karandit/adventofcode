module AoC2021.Day11
  ( aoc202111,
  )
where

import Data.List (foldl')
import qualified Data.Map as M
import Utils (readInt, (|>))

aoc202111 input = (part1, part2)
  where
    inputs = input |> lines
    energyLvls =
      inputs
        |> zip [0 ..]
        |> map (\(x, row) -> zip [0 ..] row |> map (\(y, c) -> ((x, y), readInt [c])))
        |> concat
        |> M.fromList
    adjacents (x, y) =
      [ (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1)
      ]
        |> map (\(dx, dy) -> (x + dx, y + dy))
    stepEnergyLvls (flashCount0, energyM) =
      let step1 =
            energyM
              |> M.mapAccumWithKey
                (\acc coord v -> (if v == 9 then coord : acc else acc, v + 1))
                []
          stepLast@(_, mLast) =
            until
              (\(flashes, m0) -> null flashes)
              ( \(flashes, m0) ->
                  flashes
                    |> foldl'
                      ( \acc flash ->
                          flash |> adjacents
                            |> foldl'
                              ( \acc1@(accFlashes, accM) neigh ->
                                  case M.lookup neigh accM of
                                    Nothing -> acc1
                                    Just v1 -> (if v1 == 9 then neigh : accFlashes else accFlashes, M.adjust (+ 1) neigh accM)
                              )
                              acc
                      )
                      ([], m0)
              )
              step1
          stepFinal =
            mLast
              |> M.mapAccumWithKey (\acc coord v -> if v > 9 then (acc + 1, 0) else (acc, v)) flashCount0
       in stepFinal

    part1 =
      until
        (\(idx, _) -> idx == 0)
        (\(idx, stepX) -> (idx -1, stepEnergyLvls stepX))
        (100, (0, energyLvls))
        |> snd
        |> fst

    part2 =
      until
        (\(_, (_, enMX)) -> enMX |> M.elems |> all (\x -> x == 0))
        (\(idx, stepX) -> (idx + 1, stepEnergyLvls stepX))
        (0, (0, energyLvls))
        |> fst
