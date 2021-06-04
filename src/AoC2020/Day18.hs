module AoC2020.Day18
( aoc202018a
, aoc202018b
) where

import Data.Foldable (foldl')
import Data.Char (digitToInt)
import qualified Data.Maybe as Maybe

toRPN :: (String, String) -> Char -> (String, String)
toRPN (ops, rpn) ' ' = (ops, rpn)
toRPN (ops, rpn) '(' = ('(':ops, rpn)
toRPN (ops, rpn) '+' = splitOps '+' (ops, rpn)
toRPN (ops, rpn) '*' = splitOps '*' (ops, rpn)
toRPN (ops, rpn) ')'   = splitPar (ops, rpn)
toRPN (ops, rpn) c   = (ops, c:rpn)

preced :: Char -> Int
preced '(' = 0
preced '+' = 2
preced '*' = 1

splitOps :: Char -> (String , String) -> (String, String)
splitOps op (ops, rpn) = 
   let (h, t) = splitStack ([], ops) (preced op) 
   in (op:t, h ++ rpn)

splitStack :: (String, String) -> Int -> (String, String) 
splitStack (acc, []) _ = (acc, []) 
splitStack (acc, ops@(opsH:opsT)) precOp =
     if (preced opsH > precOp) 
     then splitStack (opsH:acc, opsT) precOp
     else (acc, ops)

splitPar :: (String, String) -> (String, String)
splitPar ('(':ops, rpn) = (ops, rpn)
splitPar (op:ops, rpn) = splitPar (ops, op:rpn)

evalRPN :: [Int] -> Char -> [Int]
evalRPN (num1:num2:nums) '+' = (num1+num2):nums
evalRPN (num1:num2:nums) '*' = (num1*num2):nums
evalRPN nums c   = (digitToInt c):nums

--calc :: String -> String
calc s =
        let rpnt@(rest, rpn) = foldl' toRPN ([], "") s
            rpnf = (reverse rpn) ++ rest
        in head $ foldl' evalRPN [] rpnf
        --in (rpnt, rpnf)

main :: IO ()
main= do 
    input <- readFile "input18.txt"
    putStrLn . show . sum . map calc $ lines input

main_:: IO ()
main_ = do 
    putStrLn $ show $ calc "1 + (2 * 3) + (4 * (5 + 6))"
    putStrLn $ show $ calc "2 * 3 + (4 * 5)"
    putStrLn $ show $ calc "5 + (8 * 3 + 9 + 3 * 4 * 3)"
    putStrLn $ show $ calc "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
    putStrLn $ show $ calc "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

aoc202018a = 1
aoc202018b = 1
