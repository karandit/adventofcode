{-# LANGUAGE LambdaCase #-}
module AoC2022.Day15
  ( aoc202215,
  )
where

import Utils (readInt, (|>))
import Data.List (sortBy, find, stripPrefix)
import Data.List.Utils (split)
import qualified Data.Maybe as Maybe
import qualified Data.Map as M
import qualified Data.Range  as R

manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

aoc202215 inputY input = (part1, part2) where
    parseSensor s = s
      |> stripPrefix "Sensor at " |> Maybe.fromMaybe "" |> split ":"
      |> (\[a,b] -> (a, b |> stripPrefix " closest beacon is at " |> Maybe.fromMaybe ""))
      |> (\(a,b) -> (a |> split ", ", b |> split ", "))
      |> (\(['x':'=':sx, 'y':'=':sy], ['x':'=':bx, 'y':'=':by]) -> ((readInt sx, readInt sy),(readInt bx,readInt by)))

    diamonds = [(s, b, manhattan s b) | (s, b) <- map parseSensor $ lines $ input]

    part1 = diamonds
      |> foldl (\ranges (s@(sx, sy), b@(bx, by), rad) -> let
        ranges' = if inputY >= sy - rad && inputY <= sy + rad
               then let dx = rad - abs (sy - inputY)
                    in R.union ranges [(sx - dx) R.+=+ (sx + dx)]
               else ranges
        ranges'' = if by == inputY
              then R.difference ranges' [R.SingletonRange bx]
              else ranges'
        in ranges'') []
      |> R.fromRanges |> length

    (foundSensor@(sx,sy), _, foundRad) = diamonds |> filter (\(s1, _, rad1) -> 
                               diamonds |> find (\(s2, _, rad2) -> s1 /= s2 && manhattan s1 s2 == 2 + rad1 + rad2) |> Maybe.isJust)
                       |> sortBy (\ (s0,_, m1) (s2,_, m2) -> compare m1 m2) -- get the smallest radius diamond
                       |> head

    perimeter = foundRad + 1
    otherSensors = diamonds |> filter (\(si, _, _) -> si /= foundSensor) 

    notIncluded p = otherSensors |> any (\(si, _ , mi)-> manhattan si p <= mi) |> not

    part2 = [1..perimeter] |> map (\d -> [(sx+perimeter-d, sy+d),
                                          (sx-d, sy+perimeter-d),
                                          (sx - (perimeter-d), sy - d),
                                          (sx + d, sy - (perimeter-d))])
                   |> concat
                   |> find notIncluded
                   |> fmap(\(x, y) -> x * 4000000 + y)
                   |> Maybe.fromMaybe (-1)
