module Main where

wrapper [a,b,c] =
    let surfaces = [a*b, a*c, b*c]
    in 2 * (sum surfaces) + minimum surfaces

ribbon ns@[a,b,c] =
    let perimeters = [a+b, a+c, b+c]
    in 2 * (minimum perimeters) + product ns

main :: IO ()
main = do
    input <- readFile "input201502.txt"
    let fileLines = lines input
        readInt :: String -> Int
        readInt = read
        convertToNum line = map (readInt) $ words $ map (\c -> if c == 'x' then ' ' else c) line
        nums = map convertToNum fileLines
    print $ sum $ map ribbon nums
