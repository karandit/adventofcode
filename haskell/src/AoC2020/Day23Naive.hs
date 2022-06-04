module AoC2020.Day23Naive
  ( aoc202023a,
    aoc202023b,
  )
where

import Data.Char (digitToInt)
import Utils (debug, readInt, (|>))

destCup x1 pickeds =
  let x = if x1 == 1 then 9 else x1 -1
   in if not $ elem x pickeds
        then x
        else destCup x pickeds

move 0 cups = cups
move nr cups =
  let (x1 : p1 : p2 : p3 : xs) = cups
      dest = destCup x1 [p1, p2, p3]
      (bef, d : aft) = break (== dest) (xs ++ [x1])
   in move (nr -1) (bef ++ [d, p1, p2, p3] ++ aft)

aoc202023a input = input |> map digitToInt |> move 100

aoc202023b input = undefined

main :: IO ()
main = do
  let (x1 : p1 : p2 : p3 : xs) = "389125467" |> map digitToInt
      dest = destCup x1 [p1, p2, p3]
      (bef, d : aft) = break (== dest) (xs ++ [x1])
      sol = (bef ++ [d, p1, p2, p3] ++ aft)
  putStrLn $ show $ aoc202023a "389125467"
  putStrLn $ show $ aoc202023a "712643589"
