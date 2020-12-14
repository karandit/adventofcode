module AoC2020.Day13
( aoc202013a
, aoc202013b
) where

import Utils (readInt, replace)
import Data.List (sortBy)
import Data.Function (on)

readBus input =
  let l1:l2:_ = lines input
      tstamp = readInt l1
      busIds = words $ replace ',' ' ' l2
  in (tstamp, busIds)

sieve ((x,n):xs) = sieve' x n xs

sieve' cand step [] = cand
sieve' cand step ns@((x,n):rest) =
  if cand `mod` n == x
  then sieve' cand (lcm step n) rest
  else sieve' (cand + step) step ns

aoc202013a input =
  let (tstamp, busIds) = readBus input
      departures = map readInt . filter ((/=) "x") $  busIds
      closestDep = head
          . sortBy (compare `on` snd)
          . map (\x -> (x, (1 + (tstamp `div` x)) * x - tstamp))
          $  departures
  in fst closestDep * snd closestDep

aoc202013b input =
  let (_, busIds) = readBus input
      deps = map (\(idx, ss) -> (idx, readInt ss)) . filter ((/=) "x" .snd) . zip [0..] $ busIds
      sorted = map (\(id,x) -> ((x-id) `mod` x, x)) . reverse . sortBy (compare `on` snd) $ deps
  in sieve sorted
