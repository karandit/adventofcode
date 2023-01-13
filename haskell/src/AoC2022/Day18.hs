module AoC2022.Day18
( aoc202218
) where

import Utils (readInt, freq, (|>))
import Data.List.Utils (split)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

type XYZ = (Int, Int, Int)
type Cube = XYZ
type Face = (XYZ, XYZ)

sides :: Cube -> [Face]
sides (x, y, z) =
    [ ((x, y, z),   (x+1, y+1, z))
    , ((x, y, z+1), (x+1, y+1, z+1))

    , ((x, y, z),   (x+1, y, z+1))
    , ((x, y+1, z), (x+1, y+1, z+1))

    , ((x, y, z),   (x, y+1, z+1))
    , ((x+1, y, z),   (x+1, y+1, z+1))
    ]

adjs :: Cube -> [Cube]
adjs (x, y, z) =
    [ (x, y, z-1)
    , (x, y, z+1)
    , (x+1, y, z)
    , (x-1, y, z)
    , (x, y+1, z)
    , (x, y-1, z)
    ]

area :: Set Cube -> Int
area cubes =
    cubes |> S.toList |> map sides |> concat |> freq |> M.filter (== 1) |> length

inRange (x, y, z) ((x1, y1, z1), (x2, y2, z2)) =
     x >= x1 && x <= x2 &&
     y >= y1 && y <= y2 &&
     z >= z1 && z <= z2

aoc202218 input = (part1, part2) where
    inputs = input
              |> lines
              |> map (\s -> s |> split "," |> map readInt |> (\[x, y, z] -> (x, y, z)))

    lavaCubes :: Set Cube
    lavaCubes = inputs |> S.fromList

    part1 = lavaCubes |> area

    minX = inputs |> map (\(x, _, _) -> x) |> minimum |> pred
    minY = inputs |> map (\(_, y, _) -> y) |> minimum |> pred
    minZ = inputs |> map (\(_, _, z) -> z) |> minimum |> pred
    maxX = inputs |> map (\(x, _, _) -> x) |> maximum |> succ
    maxY = inputs |> map (\(_, y, _) -> y) |> maximum |> succ
    maxZ = inputs |> map (\(_, _, z) -> z) |> maximum |> succ
    dX = maxX - minX + 1
    dY = maxY - minY + 1
    dZ = maxZ - minZ + 1

    floodFill :: (XYZ, XYZ) -> Set Cube -> Set Cube
    floodFill range@(minXYZ, _) lava = go S.empty [minXYZ] where
        go visited []     = visited
        go visited q@(x:xs) = let
              neighs = x |> adjs
                  |> filter(\k -> S.notMember k lava)
                  |> filter(\k -> S.notMember k visited)
                  |> filter(\k -> k `notElem` q)
                  |> filter(\k -> inRange k range )
          in go (S.insert x visited) (xs ++ neighs)

    part2 = let
      steam = floodFill ((minX, minY, minZ), (maxX, maxY, maxZ)) lavaCubes
      in (area steam) - 2 * (dX * dY +  dX * dZ +  dY *dZ)
