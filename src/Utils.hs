module Utils
( readInt
, replace
, freq
, (?)
, Cond (..)
) where

import qualified Data.Map as M

readInt :: String -> Int
readInt = read

replace fromC toC = map (\c -> if c == fromC then toC else c)

freq s = M.fromListWith (+) [(c, 1) | c <- s]
-- ternary operator
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
