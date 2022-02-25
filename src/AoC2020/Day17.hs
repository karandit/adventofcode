module AoC2020.Day17
  ( aoc202017,
  )
where

import Data.Foldable (foldl')
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import Utils

readCubes :: String -> S.Set (Int, Int)
readCubes input =
  foldl'
    ( \acc1 (x, row) ->
        foldl' (\acc2 (y, c) -> if c == '#' then S.insert (x, y) acc2 else acc2) acc1 $
          zip [0 ..] row
    )
    S.empty
    $ zip [0 ..] $ lines input

stepGen :: Ord z => (z -> z -> z) -> S.Set (Int, Int, z) -> S.Set (Int, Int, z) -> S.Set (Int, Int, z)
stepGen shiftZ deltaZ cubes =
  let shift3D (x, y, z) (dx, dy, dz) = (x + dx, y + dy, shiftZ z dz)
      perimeter cube = S.toList $ S.map (shift3D cube) deltaZ
      neighbours = freq $ concat $ S.toList $ S.map perimeter cubes
   in M.foldlWithKey
        ( \acc pos occurences ->
            case occurences of
              2 -> if S.member pos cubes then S.insert pos acc else acc
              3 -> S.insert pos acc
              _ -> acc
        )
        S.empty
        neighbours

grid3D :: [(Int, Int, Int)]
grid3D = [(x, y, z) | x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1]]

grid4D :: [(Int, Int, (Int, Int))]
grid4D = [(x, y, (z, w)) | x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1], w <- [-1, 0, 1]]

shift3D :: Int -> Int -> Int
shift3D z dz = z + dz

shift4D :: (Int, Int) -> (Int, Int) -> (Int, Int)
shift4D (z, w) (dz, dw) = (z + dz, w + dw)

solution :: Ord z => (z -> z -> z) -> [(Int, Int, z)] -> z -> String -> Int
solution shiftZ gridZ origoZ input =
  let cubes = S.map (\(x, y) -> (x, y, origoZ)) $ readCubes input
      deltaZ = S.difference (S.fromList gridZ) (S.singleton (0, 0, origoZ))
      step = stepGen shiftZ deltaZ
   in S.size $ step $ step $ step $ step $ step $ step $ cubes

aoc202017 input = (part1 input, part2 input)
  where
    part1 = solution shift3D grid3D 0
    part2 = solution shift4D grid4D (0, 0)
