module AoC2020.Day01
( aoc202001a
, aoc202001b
) where

import Data.List (tails)

aoc202001a nums = head $ [x*y | x:ys <- tails nums, y <- ys, x + y == 2020]
aoc202001b nums = head $ [x*y*z | x:ys <- tails nums, y:zs <- tails ys, z <- zs, x + y + z == 2020]
