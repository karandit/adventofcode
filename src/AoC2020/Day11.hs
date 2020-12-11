module AoC2020.Day11
( aoc202011a
, aoc202011b
) where

import qualified Data.Map as M

readSeats input =
    foldl (\acc1 (rIdx, row) ->
        foldl (\acc2 (cIdx, seat) -> 
                M.insert (rIdx, cIdx) seat acc2
        ) acc1 $ zip [0..] row
    ) M.empty  $ zip [0..] $ lines input   

initns = [
      (-1, -1), (-1, 0), (-1, 1),
      ( 0, -1),          ( 0, 1),
      ( 1, -1), ( 1, 0), ( 1, 1) ]

shiftPos (x,y) (dx, dy) = (x+dx, y+dy)

moveAway (x,y) = (
    if x<0 then x-1 else if x>0 then x+1 else x, 
    if y<0 then y-1 else if y>0 then y+1 else y
    )

nrNeighboursPart1 pos store =
    length $ filter (\d -> maybe False (\c -> c == '#') $ M.lookup (shiftPos pos d) store) initns

nrNeighboursPart2 pos store = 
    nrNeighboursPart2' 0 initns pos store

nrNeighboursPart2' acc [] pos store = acc
nrNeighboursPart2' acc ns pos store = 
    let foundSeats = map (\d -> (M.lookup (shiftPos pos d) store, d)) ns
        seen = length $ filter (\(p,d) -> maybe False (\c -> c == '#') p) foundSeats
        nextNs = map (moveAway .snd) $ filter (\(p,d) -> case p of
                                Just '.' -> True
                                _  -> False) foundSeats
    in nrNeighboursPart2' (acc+seen) nextNs pos store

-- common
occupied store =
    length $ filter((==)'#') $ M.elems store

step neighbours toler store =
    M.foldlWithKey (\acc pos c ->
        let nextC =  case c of
                        '.' -> c
                        'L' -> if neighbours pos store == 0     then '#' else c
                        '#' -> if neighbours pos store >= toler then 'L' else c
        in M.insert pos nextC acc
        ) M.empty store

stepInf neightbours toler store =
    let nextStore = step neightbours toler store
    in if nextStore == store then occupied store else stepInf neightbours toler nextStore

aoc202011a input = stepInf nrNeighboursPart1 4 $ readSeats input
aoc202011b input = stepInf nrNeighboursPart2 5 $ readSeats input
