module Utils
( readInt
, applyN
, replace
, freq
, replaceNth
, dec2bin
, bin2dec
, insUpd
, debug
, (|>), (?)
, Cond (..)
) where

import qualified Data.Map as M
import qualified Data.Char as Char
import Data.List
import Debug.Trace (trace)

readInt :: String -> Int
readInt = read

applyN n f = foldr (.) id $ replicate n f

replace fromC toC = map (\c -> if c == fromC then toC else c)

freq s = M.fromListWith (+) [(c, 1) | c <- s]

replaceNth pos newVal list = take pos list ++ newVal : drop (pos+1) list

dec2bin :: Int -> [Char]
dec2bin = reverse . map Char.intToDigit . unfoldr
    (\x -> if x==0 then Nothing else Just(rem x 2, div x 2))

bin2dec = sum . map (2^) . findIndices (=='1') . reverse

insUpd store k val = case M.lookup k store of
  Just _  -> M.update (\_ -> Just val) k store
  Nothing -> M.insert k val store


debug s a = trace (s ++ ":" ++ (show a)) a
-- pipe operator
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- ternary operator
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
