module Utils
( readInt
, replace
, freq
, replaceNth
, debug
, (?)
, Cond (..)
) where

import qualified Data.Map as M
import Debug.Trace (trace)

readInt :: String -> Int
readInt = read

replace fromC toC = map (\c -> if c == fromC then toC else c)

freq s = M.fromListWith (+) [(c, 1) | c <- s]

replaceNth pos newVal list = take pos list ++ newVal : drop (pos+1) list

debug s a = trace (s ++ ":" ++ (show a)) a
-- ternary operator
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
