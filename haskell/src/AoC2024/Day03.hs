{-# Language OverloadedStrings #-}
module AoC2024.Day03
( aoc202403
) where

import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Foldable (foldl')

data Instr = Mul Int Int | Do | Dont | Junk deriving Show

pInstrs = many (
      (Mul <$> ("mul(" *> decimal <* ",") <*> (decimal <* ")")) <|>
      (Do <$ "do()") <|>
      (Dont <$ "don't()") <|>
      (Junk <$ anySym)
  )

aoc202403 input = (part1, part2) where
  Just terms = match pInstrs input
  part1 = sum [a * b | Mul a b <- terms]
  (_, part2) = foldl' (\acc@(isOk, res) term -> case term of
                          Dont -> (False, res)
                          Do   -> (True, res)
                          Mul a b -> (isOk, res + (if isOk then a * b else 0))
                          Junk -> acc)
                (True, 0) terms
