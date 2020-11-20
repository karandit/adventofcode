module Main where

direction c = case c of
  '(' -> 1
  ')' -> -1
  otherwise  -> 0

main :: IO ()
main = do
    raw <- readFile "input201501.txt"
    let directions = map direction raw
    let gold = sum $ directions
    let silver = length $ takeWhile (> -1) $ scanl (+) 0 directions
    print (gold, silver)
