{-# LANGUAGE LambdaCase #-}

module AoC2022.Day13
  ( aoc202213,
  )
where

import Data.List (elemIndex, intercalate, sortBy)
import Data.List.Utils (split)
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

data Packet = PNr Int | PList [Packet] deriving (Eq)

instance Show Packet where
  show = \case
    PNr n -> show n
    PList items -> "[" ++ (intercalate "," $ map show $ items) ++ "]"

aoc202213 input = (part1, part2)
  where
    packets :: [(Packet, Packet)]
    packets = input |> lines |> split [""] |> map (\[a, b] -> (parsePacket a, parsePacket b))
    parsePacket s = packet where ([packet], "") = parse_ [] s

    parse_ acc = \case
      "" -> (acc, "")
      '[' : xs -> let (items, xs') = parse_ [] xs in parse_ (acc ++ [PList items]) xs'
      ',' : xs -> parse_ acc xs
      ']' : xs -> (acc, xs)
      '1' : '0' : xs -> parse_ (acc ++ [PNr 10]) xs
      d : xs -> parse_ (acc ++ [PNr (readInt [d])]) xs

    isRightOrder :: Packet -> Packet -> Ordering
    isRightOrder (PNr l) (PNr r) = compare l r
    isRightOrder lNr@(PNr _) rList@(PList _) = isRightOrder (PList [lNr]) rList
    isRightOrder lList@(PList _) rNr@(PNr _) = isRightOrder lList (PList [rNr])
    isRightOrder (PList []) (PList []) = EQ
    isRightOrder (PList []) (PList _) = LT
    isRightOrder (PList _) (PList []) = GT
    isRightOrder (PList (l : ls)) (PList (r : rs)) =
      let f1 = isRightOrder l r
       in if f1 /= EQ then f1 else isRightOrder (PList ls) (PList rs)

  --part1 = packets |> zip [1 ..] |> filter (\(i, (a, b)) -> isRightOrder a b == LT) |> map fst |> sum
    part1 = [i | (i, (a, b)) <- zip [1 ..] packets, isRightOrder a b == LT] |> sum

    divider1 = PList [PList [PNr 2]]
    divider2 = PList [PList [PNr 6]]

  --packetsOrdered = packets |> map (\(a, b) -> [a, b]) |> concat |> (++) [divider1, divider2] |> sortBy isRightOrder
    packetsOrdered = divider1 : divider2 : [ab | (a, b) <- packets, ab <- [a, b]] |> sortBy isRightOrder

    idxDiv1 = packetsOrdered |> elemIndex divider1 |> fmap (+ 1)
    idxDiv2 = packetsOrdered |> elemIndex divider2 |> fmap (+ 1)
    part2 = Maybe.fromMaybe (-1) $ Just (*) <*> idxDiv2 <*> idxDiv1
