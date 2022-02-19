module AoC2020.Day22
( aoc202022
) where

import Data.List.Utils (split)
import qualified Data.Set as S
import Utils ((|>), readInt)

parseDecks :: String -> ([Int], [Int])
parseDecks input =
    let player1:player2:[] = input |> lines |> split [""]
        deck1 = player1 |> tail |> map readInt
        deck2 = player2 |> tail |> map readInt
    in (deck1, deck2)

nextDecksBy isFirst (x1:xs, y1:ys) =
    if isFirst
    then ((xs ++ [x1, y1]), ys)
    else (xs, (ys ++ [y1, x1]))

nextDecks decks@(x1:xs, y1:ys) =
    nextDecksBy (x1 > y1) decks

combatSimple (xs, []) = xs
combatSimple ([], ys) = ys
combatSimple (deckx, decky) = (deckx, decky) |> nextDecks |> combatSimple

combatRecurs decks = decks |> combatRec S.empty S.empty |> snd

combatRec _ _ (xs, []) = (True, xs)
combatRec _ _ ([], ys) = (False, ys)
combatRec memo1 memo2 decks@(d1@(x1:xs), d2@(y1:ys)) =
    if S.member d1 memo1 || S.member d2 memo2
    then (True, [])
    else if x1 <= length xs && y1 <= length ys
          then let (isFirst,_) = combatRec S.empty S.empty ((take x1 xs), (take y1 ys))
               in decks |> nextDecksBy isFirst |> combatRec (S.insert d1 memo1) (S.insert d2 memo2)
          else    decks |> nextDecks           |> combatRec (S.insert d1 memo1) (S.insert d2 memo2)

calcCards cards = cards |> reverse |> zipWith (*) [1..] |> sum

aoc202022 input = (part1, part2) where
  part1 = input |> parseDecks |> combatSimple |> calcCards
  part2 = input |> parseDecks |> combatRecurs |> calcCards
