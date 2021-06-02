module AoC2020.Day17
( aoc202017a
, aoc202017b
) where

import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import qualified Data.Map as M
import Utils

readCubes :: String -> S.Set (Int, Int)
readCubes input =
    foldl' (\acc1 (x, row) ->
        foldl' (\acc2 (y, c) -> if c == '#' then S.insert (x, y) acc2 else acc2) acc1
        $ zip [0..] row
    ) S.empty  $ zip [0..] $ lines input

stepGen :: Ord z => (z -> z -> z) -> [(Int, Int, z)] -> S.Set (Int, Int, z) -> S.Set (Int, Int, z)
stepGen shiftZ deltaZ cubes =
    let
        shift3D (x, y, z) (dx, dy, dz) = (x + dx, y + dy, shiftZ z dz)
        perimeter cube = fmap (shift3D cube) deltaZ
        neighbours = freq $ concat $ S.foldl' (\acc cube -> (perimeter cube):acc) [] cubes
    in
    M.foldlWithKey (\acc pos occurences ->
            case occurences of
              2 -> if S.member pos cubes then S.insert pos acc else acc 
              3 -> S.insert pos acc
              _ -> acc
    ) S.empty neighbours

delta2D :: [(Int, Int)]
delta2D = [
      (-1, -1), (-1, 0), (-1, 1),
      ( 0, -1), ( 0, 0), ( 0, 1),
      ( 1, -1), ( 1, 0), ( 1, 1)
    ]

delta3D :: [(Int, Int, Int)]
delta3D = [
      (-1, -1, -1), (-1, 0, -1), (-1, 1, -1),
      ( 0, -1, -1), ( 0, 0, -1), ( 0, 1, -1),
      ( 1, -1, -1), ( 1, 0, -1), ( 1, 1, -1),

      (-1, -1, 0), (-1, 0, 0), (-1, 1, 0),
      ( 0, -1, 0),             ( 0, 1, 0),
      ( 1, -1, 0), ( 1, 0, 0), ( 1, 1, 0),

      (-1, -1, 1), (-1, 0, 1), (-1, 1, 1),
      ( 0, -1, 1), ( 0, 0, 1), ( 0, 1, 1),
      ( 1, -1, 1), ( 1, 0, 1), ( 1, 1, 1)
        ]

delta4D :: [(Int, Int, (Int, Int))]
delta4D = [
      --w=-1
      (-1, -1, (-1, -1)), (-1, 0, (-1, -1)), (-1, 1, (-1, -1)),
      ( 0, -1, (-1, -1)), ( 0, 0, (-1, -1)), ( 0, 1, (-1, -1)),
      ( 1, -1, (-1, -1)), ( 1, 0, (-1, -1)), ( 1, 1, (-1, -1)),

      (-1, -1, (0, -1)), (-1, 0, (0, -1)), (-1, 1, (0, -1)),
      ( 0, -1, (0, -1)), ( 0, 0, (0, -1)), ( 0, 1, (0, -1)),
      ( 1, -1, (0, -1)), ( 1, 0, (0, -1)), ( 1, 1, (0, -1)),

      (-1, -1, (1, -1)), (-1, 0, (1, -1)), (-1, 1, (1, -1)),
      ( 0, -1, (1, -1)), ( 0, 0, (1, -1)), ( 0, 1, (1, -1)),
      ( 1, -1, (1, -1)), ( 1, 0, (1, -1)), ( 1, 1, (1, -1)),
      --w=0
      (-1, -1, (-1, 0)), (-1, 0, (-1, 0)), (-1, 1, (-1, 0)),
      ( 0, -1, (-1, 0)), ( 0, 0, (-1, 0)), ( 0, 1, (-1, 0)),
      ( 1, -1, (-1, 0)), ( 1, 0, (-1, 0)), ( 1, 1, (-1, 0)),

      (-1, -1, (0, 0)), (-1, 0, (0, 0)), (-1, 1, (0, 0)),
      ( 0, -1, (0, 0)),                  ( 0, 1, (0, 0)),
      ( 1, -1, (0, 0)), ( 1, 0, (0, 0)), ( 1, 1, (0, 0)),

      (-1, -1, (1, 0)), (-1, 0, (1, 0)), (-1, 1, (1, 0)),
      ( 0, -1, (1, 0)), ( 0, 0, (1, 0)), ( 0, 1, (1, 0)),
      ( 1, -1, (1, 0)), ( 1, 0, (1, 0)), ( 1, 1, (1, 0)),
      --w=1
      (-1, -1, (-1, 1)), (-1, 0, (-1, 1)), (-1, 1, (-1, 1)),
      ( 0, -1, (-1, 1)), ( 0, 0, (-1, 1)), ( 0, 1, (-1, 1)),
      ( 1, -1, (-1, 1)), ( 1, 0, (-1, 1)), ( 1, 1, (-1, 1)),

      (-1, -1, (0, 1)), (-1, 0, (0, 1)), (-1, 1, (0, 1)),
      ( 0, -1, (0, 1)), ( 0, 0, (0, 1)), ( 0, 1, (0, 1)),
      ( 1, -1, (0, 1)), ( 1, 0, (0, 1)), ( 1, 1, (0, 1)),

      (-1, -1, (1, 1)), (-1, 0, (1, 1)), (-1, 1, (1, 1)),
      ( 0, -1, (1, 1)), ( 0, 0, (1, 1)), ( 0, 1, (1, 1)),
      ( 1, -1, (1, 1)), ( 1, 0, (1, 1)), ( 1, 1, (1, 1))

        ]

shift3D :: Int -> Int -> Int
shift3D z dz = z + dz

shift4D :: (Int, Int) -> (Int, Int) -> (Int, Int)
shift4D (z, w) (dz, dw) = (z + dz, w + dw)

solution :: Ord z => (z -> z -> z) -> [(Int, Int, z)] -> z -> String -> Int
solution shiftZ deltaZ z input =
    let cubes = S.map (\(x,y) -> (x, y, z)) $ readCubes input
        step = stepGen shiftZ deltaZ
    in S.size $ step $ step $ step $ step $ step $ step $ cubes

aoc202017a = solution shift3D delta3D 0 
aoc202017b = solution shift4D delta4D (0, 0)
