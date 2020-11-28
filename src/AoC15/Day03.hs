module AoC15.Day03
( aoc201503a
, aoc201503b )
where

import qualified Data.Set as Set

move c pos@(x, y) = case c of
  'v' -> (x    , y + 1)
  '^' -> (x    , y - 1)
  '>' -> (x + 1, y    )
  '<' -> (x - 1, y    )
  otherwise -> pos

initialPos = ((0,0), Set.singleton (0, 0))
stepPos (pos, visited) c =
                    let nextPos = move c pos 
                    in (nextPos, Set.union (Set.singleton nextPos) visited)

aoc201503a input = Set.size $ snd $ foldl stepPos initialPos input

aoc201503b input = let 
                  (_, resS, resR) = foldl (\(isS, posS, posR) c -> 
                    let nextS = if     isS then stepPos posS c else posS
                        nextR = if not isS then stepPos posR c else posR
                    in (not isS, nextS, nextR)) (True, initialPos, initialPos) input
               in Set.size $ Set.union (snd resS) (snd resR)
