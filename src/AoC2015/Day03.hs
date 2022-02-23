module AoC2015.Day03
( aoc201503
) where

import qualified Data.Set as Set

move 'v' (x, y) = (x    , y + 1)
move '^' (x, y) = (x    , y - 1)
move '>' (x, y) = (x + 1, y    )
move '<' (x, y) = (x - 1, y    )
move _   pos    = pos

initialPos = ((0,0), Set.singleton (0, 0))
stepPos (pos, visited) c =
                    let nextPos = move c pos 
                    in (nextPos, Set.union (Set.singleton nextPos) visited)

-- | Part 1
-- >>> part1 ">"
-- 2
-- >>> part1 "^>v<"
-- 4
-- >>> part1 "^v^v^v^v^v"
-- 2
part1 input = Set.size $ snd $ foldl stepPos initialPos input

-- | Part2
-- >>> part2 "^v"
-- 3
-- >>> part2 "^>v<"
-- 3
-- >>> part2 "^v^v^v^v^v"
-- 11
part2 input =
  let (_, resS, resR) = foldl (\(isS, posS, posR) c ->
       let nextS = if     isS then stepPos posS c else posS
           nextR = if not isS then stepPos posR c else posR
       in (not isS, nextS, nextR)) (True, initialPos, initialPos) input
  in Set.size $ Set.union (snd resS) (snd resR)

aoc201503 input = (part1 input, part2 input)
