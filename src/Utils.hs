module Utils
( readInt
, replace
, freq
) where

import qualified Data.Map as M

readInt :: String -> Int
readInt = read

replace fromC toC = map (\c -> if c == fromC then toC else c)

freq s = M.fromListWith (+) [(c, 1) | c <- s]
