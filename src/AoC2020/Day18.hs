module AoC2020.Day18
( aoc202018
, calc1
, calc2
) where

import Data.Foldable (foldl')
import Data.List (break)
import Data.Char (digitToInt)

toRPN :: (Char -> Int) -> (String, String) -> Char -> (String, String)
toRPN precedence (rpn, ops) c =
  let
    handleOp :: Char -> (String , String) -> (String, String)
    handleOp op (rpn, ops) =
      let precCurrOp = precedence op
          (bef, aft) = break (\o -> precedence o < precCurrOp) ops
      in ((reverse bef) ++ rpn, op:aft)

    handlePar :: (String, String) -> (String, String)
    handlePar (rpn, ops) =
      let (bef, '(':aft) = break (=='(') ops
      in ((reverse bef) ++ rpn, aft)
  in
    case c of
      ' ' -> (rpn, ops)
      '+' -> handleOp c (rpn, ops)
      '*' -> handleOp c (rpn, ops)
      '(' -> (rpn, c:ops)
      ')' -> handlePar (rpn, ops)
      dig -> (dig:rpn, ops)

evalRPN :: [Int] -> Char -> [Int]
evalRPN (num1:num2:nums) '+' = (num1+num2):nums
evalRPN (num1:num2:nums) '*' = (num1*num2):nums
evalRPN nums c               = (digitToInt c):nums

calc :: (Char -> Int) -> String -> Int
calc precedence s =
  let (rpnTemp, ops) = foldl' (toRPN precedence) ("", "") s
      rpn = (reverse rpnTemp) ++ ops
  in head $ foldl' evalRPN [] rpn

precedence1 :: Char -> Int
precedence1 '(' = 0
precedence1 '+' = 1
precedence1 '*' = 1

precedence2 :: Char -> Int
precedence2 '(' = 0
precedence2 '+' = 2
precedence2 '*' = 1

calc1 = calc precedence1
calc2 = calc precedence2

aoc202018 input = (part1, part2) where
  part1 = sum . map calc1 . lines $ input
  part2 = sum . map calc2 . lines $ input
