module AoC2023.Day02
( aoc202302
) where

import Utils (readInt)
import Data.List.Utils (split)
import Data.Foldable (foldl')

parseGame :: String -> (Int, [[ (Int, String) ]])
parseGame s = let
    [gameTitle, rounds] = split ":" s
    ["Game", gameId] = split " " gameTitle
    game = [[parseCube . tail . split " " $ cube | cube <- split "," round] | round <- split ";" rounds ]
    in (readInt gameId, game)

parseCube [nr, color] = (readInt nr, color)

predCube (n, "red")   = n <= 12
predCube (n, "green") = n <= 13
predCube (n, "blue")  = n <= 14

addCube (r, g, b) (n, "red")   = (max r n, g, b)
addCube (r, g, b) (n, "green") = (r, max g n, b)
addCube (r, g, b) (n, "blue")  = (r, g, max b n)

aoc202302 input = (part1, part2) where
  games = [parseGame g | g <- lines input]

  part1 = sum [gid | (gid, rounds) <- games, all (all predCube) rounds]
  part2 = sum [r * g * b | (r, g, b) <- [ foldl' (foldl' addCube) (0, 0, 0) rounds | (_, rounds) <- games ]]
