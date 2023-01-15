module AoC2022.Day22
( aoc202222
) where

import Data.List (foldl', scanl', intercalate)
import Data.List.Utils (split)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

data Inst = M Int | R | L deriving (Show)

inRange (x, y) (x1, y1) (x2, y2) = x >= x1 && x <=x2 && y >= y1 && y <= y2

right = 0  
down = 1
left =2
up = 3

step 0 (x, y) = (x+1, y)
step 1 (x, y) = (x, y+1)
step 2 (x, y) = (x-1, y)
step 3 (x, y) = (x, y-1)

aoc202222 isSmallInput input = (part1, part2) where
    inputs = input |> lines
    instrs = inputs |> last
            |> split "R"
            |> map (\s -> s
                          |> split "L"
                          |> map (\s1 -> [M (readInt s1)])
                          |> intercalate [L])
            |> intercalate [R]

    coords = [((x, y), v) | (y, row) <- zip [1..] $ init $ init $ inputs, (x, v) <- zip [1..] row, v /= ' ']
    startx = [x | ((x, y), v) <- coords,  y == 1 &&  v == '.'] |> minimum
    board = coords |> M.fromList

    wrap1 xy f =
        let stepf  = step ((f + 2) `mod` 4)
        in (until (\p -> M.notMember (stepf p) board) stepf xy, f)

    move wrapper (pos, facing, _, acc) R      = (pos, (facing + 1) `mod` 4, R, [])
    move wrapper (pos, facing, _, acc) L      = (pos, (facing - 1) `mod` 4, L, [])
    move wrapper (pos, facing, _, acc) (M l)  = (newpos, newf, M l, reverse trav) where
        (newpos, newf, trav)= go pos l facing []
        go xy 0 f acc= (xy, f, acc)
        go xy n f acc= let nextxy = step f xy
                  in case M.lookup nextxy board of
                        Nothing -> let (edgexy, f') = wrapper xy f
                                   in case M.lookup edgexy board of
                                       Nothing -> error "nothing found after wrap"
                                       Just '#' -> (xy, f, acc)
                                       Just '.' -> go edgexy (n-1) f' (edgexy:acc)
                        Just '#' -> (xy, f, acc)
                        Just '.' -> go nextxy (n-1) f (nextxy:acc)

    solve wrapper = foldl' (move wrapper) ((startx, 1), 0, R, []) instrs
               |> (\((x, y), f, _, _) -> 1000 * y + 4 * x + f)
    part1= solve wrap1

    wrap2Small p@(x,y) f
        | inRange p ( 9, 1) (12,  1) && f == up    = (( 5 - (x - 8), 5), down)
        | inRange p ( 1, 5) ( 4,  5) && f == up    = (( 8 +  (x - 4), 1), down)

        | inRange p ( 5, 5) (8,  5) &&  f == up    = ((9, 0 +  (x - 4)), right)
        | inRange p (9, 1) (9, 4)   && f == left   = ((4 +  (y - 0), 5), down)

        | inRange p (5, 8) (8, 8)  && f == down    = ((9, 13 - (x - 4)), right)
        | inRange p (9, 9) (9, 12) && f == left    = ((4 +  (13 - y), 8), up)

        | inRange p (13, 9) (16, 9) && f == up    = ((12, 13 - (x - 12)), left)
        | inRange p (12, 5) (12, 8) && f == right = ((12 +  (9 - y), 9), down)

        | inRange p (12, 1) (12, 4) && f == right    = ((16, 8+ (5- y)), left)
        | inRange p (16, 9) (16, 12) && f == right    = ((12, 0+ (13- y)), left)

        | inRange p ( 1, 8) ( 4,  8) && f == down    = (( 8 +  (5 - x), 12), up)
        | inRange p (9, 12) (12, 12) && f == down    = (( 0 +  (13 - x), 8), up)

        | inRange p ( 1, 5) (1, 8) &&  f == left    = ((12 +  (9-y), 12), up)
        | inRange p (13, 12) (16, 12)  && f == down    = ((1, 4 + (17-x)), right)
        | otherwise = wrap1 p f

    wrap2Big p@(x,y) f
        | inRange p (1, 101) (50, 101) && f == up      = ((51, 50 + x), right)
        | inRange p (51, 51) (51, 100) && f == left    = ((0 + (y - 50), 101), down)

        | inRange p (101, 50) (150, 50) && f == down   = ((100, 50 + (x-100)), left)
        | inRange p (100, 51) (100, 100)&& f == right  = ((100 + (y-50), 50) , up)

        | inRange p (51, 150) (100, 150) && f == down   = ((50, 150 + (x - 50)), left)
        | inRange p (50, 151) (50, 200) && f == right  = ((50 + (y - 150), 150) , up)

        | inRange p (51, 1) (51, 50) && f == left   = ((1, 100 + (51 -y)), right)
        | inRange p (1, 101) (1, 150) && f == left   = ((51, 0 + (151 -y)), right)

        | inRange p (51, 1) (100, 1) && f == up      = ((1, 150 + (x - 50)), right)
        | inRange p (1, 151) (1, 200) && f == left      = ((50 + (y - 150), 1), down)

        | inRange p (150, 1) (150, 50) && f == right  = ((100, 100 + (51 - y)) , left)
        | inRange p (100, 101) (100, 150)&& f == right  = ((150, 0 + (151 - y)) , left)

        | inRange p (101, 1) (150, 1) && f == up      = ((0 + (x- 100), 200), up)
        | inRange p (1, 200) (50, 200) && f == down      = ((100 + x, 1), down)
        | otherwise = wrap1 p f

    part2 = solve (if isSmallInput then wrap2Small else wrap2Big)
