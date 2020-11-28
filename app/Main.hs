module Main where

--import qualified Data.Set as Set
--
--move c pos@(x, y) = case c of
--  'v' -> (x    , y + 1)
--  '^' -> (x    , y - 1)
--  '>' -> (x + 1, y    )
--  '<' -> (x - 1, y    )
--  otherwise -> pos
--
--initialPos = ((0,0), Set.singleton (0, 0))
--step (pos, visited) c =
--                    let nextPos = move c pos 
--                    in (nextPos, Set.union (Set.singleton nextPos) visited)
--
--gold input = let res = foldl step initialPos input
--             in Set.size $ snd res
--
--silver input = let 
--                  (_, resS, resR) = foldl (\(isS, posS, posR) c -> 
--                    let nextS = if isS then step posS c else posS
--                        nextR = if not isS then step posR c else posR
--                    in (not isS, nextS, nextR)) (True, initialPos, initialPos) input
--               in Set.size $ Set.union (snd resS) (snd resR)
--
main :: IO ()
main = do
    input <- readFile "input201503.txt"
    print $ input
