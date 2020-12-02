module AoC2015.Day02
( aoc201502a
, aoc201502b
, parseInput201502)
where

aoc201502a [a,b,c] = 
    let surfaces = [a*b, a*c, b*c]    
    in 2 * (sum surfaces) + minimum surfaces

aoc201502b ns@[a,b,c] =
    let perimeters = [a+b, a+c, b+c]    
    in 2 * (minimum perimeters) + product ns
    
parseInput201502 input =
    let fileLines = lines input

        readInt :: String -> Int
        readInt = read

        readNums line = map (readInt) $ words $ map (\c -> if c == 'x' then ' ' else c) line
    in  map readNums fileLines


