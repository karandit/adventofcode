module AoC2023.Day07
( aoc202307
) where

import Utils (readInt, freq, (|>))
import Data.List (sortBy)
import qualified Data.Map as M

data HandType = Highest | One | Two | Three| Full | Four | Five deriving (Show, Eq, Ord)

compareHands fCard (hand1, typ1, _) (hand2, typ2, _) = 
  let ordType = compare typ1 typ2
 
      compareCards [] [] = EQ
      compareCards (a1:as) (b1:bs) = 
          let ordC = compare (fCard a1) (fCard b1)
          in if ordC == EQ then compareCards as bs else ordC
 
  in if ordType == EQ then compareCards hand1 hand2 else ordType

evalHand1 :: String -> HandType
evalHand1 hand = case (freq hand |> M.toList |> map snd |> freq |> M.toList) of
  [(5, 1)]         -> Five
  [(1, 1), (4, 1)] -> Four
  [(2, 1), (3, 1)] -> Full
  [(1, 2), (3, 1)] -> Three
  [(1, 1), (2, 2)] -> Two
  [(1, 3), (2, 1)] -> One
  [(1, 5)]         -> Highest

cards = zip "23456789TJQKA" [2..] |> M.fromList

evalCard1 c = cards M.! c

evalHand2 :: String -> HandType
evalHand2 hand = case (length [c | c <- hand, c == 'J'], evalHand1 hand) of
  (0,  typ    ) -> typ
  (_,  Five   ) -> Five
  (_,  Four   ) -> Five
  (_,  Full   ) -> Five
  (_,  Three  ) -> Four
  (1,  Two    ) -> Full
  (2,  Two    ) -> Four
  (_,  One    ) -> Three
  (_,  Highest) -> One

evalCard2 c = if c == 'J' then 1 else evalCard1 c

aoc202307 input = (part1, part2) where
  solve fHand fCard = input
    |> lines
    |> map (\s -> words s |> (\[hand, bid] -> (hand, fHand hand, readInt bid)))
    |> sortBy (compareHands fCard)
    |> zip [1..]
    |> map (\(rank, (_, _, bid)) -> rank * bid)
    |> sum

  part1 = solve evalHand1 evalCard1
  part2 = solve evalHand2 evalCard2
