module AoC2022.Day21
( aoc202221
) where

import Utils (readInt, (|>))
import Data.List (foldl')
import Data.List.Utils (split)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

data R = V Int | Ref String | Pls R R | Mns R R | Mul R R | Div R R | Guess | Match R R deriving (Show)

aoc202221 :: String -> (Int, Int)
aoc202221 input = (part1, part2) where

  parse s = s |> split ": " |> (\[name,op] -> (name, parseOp op))

  parseOp s
     | '+' `elem` s = s |> split " + " |> (\[a,b] -> Pls (Ref a) (Ref b))
     | '-' `elem` s = s |> split " - " |> (\[a,b] -> Mns (Ref a) (Ref b))
     | '/' `elem` s = s |> split " / " |> (\[a,b] -> Div (Ref a) (Ref b))
     | '*' `elem` s = s |> split " * " |> (\[a,b] -> Mul (Ref a) (Ref b))
     | otherwise    = V (readInt s) 

  monkeys = input |> lines |> map parse |> M.fromList

  eval1 :: Map String R -> R -> Int
  eval1 m r = eval' r
      where 
      eval' (V v)     = v
      eval' (Ref s)   = m M.! s |> eval'
      eval' (Pls a b) = (eval' a) +     (eval' b)
      eval' (Mns a b) = (eval' a) -     (eval' b)
      eval' (Mul a b) = (eval' a) *     (eval' b)
      eval' (Div a b) = (eval' a) `div` (eval' b)

  root = monkeys M.! "root"
  part1 = eval1 monkeys root

  decide :: Maybe a -> Maybe a -> Maybe a
  decide a@(Just _) b@(Just _) = error "Something went wrong"
  decide a@(Just _) Nothing = a
  decide Nothing b@(Just _) = b
  decide Nothing Nothing = Nothing

  eval2 :: Map String R -> R -> Maybe R
  eval2 m r = eval' (V 0) r 
      where
      eval' _   (Match a b) = decide (eval' b a) (eval' a b)
      eval' acc Guess       = Just acc
      eval' acc (V v)       = Nothing
      eval' acc (Ref s)     = m M.! s |> eval' acc 
      eval' acc (Pls a b)   = decide (eval' (Mns acc b) a) (eval' (Mns acc a) b)
      eval' acc (Mns a b)   = decide (eval' (Pls acc b) a) (eval' (Mns a acc) b)
      eval' acc (Mul a b)   = decide (eval' (Div acc b) a) (eval' (Div acc a) b)
      eval' acc (Div a b)   = decide (eval' (Mul acc b) a) (eval' (Div a acc) b)

  monkeys2 = monkeys |> M.update (const $ Just Guess) "humn"
                     |> M.update (\(Pls a b) -> Just (Match a b)) "root" 

  root2 = monkeys2 M.! "root"
  part2 = eval2 monkeys2 root2 |> fmap (eval1 monkeys2) |> Maybe.fromMaybe (-1)
