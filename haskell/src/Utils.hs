module Utils
  ( readInt,
    applyN,
    replace,
    freq,
    replaceNth,
    chunksOf,
    dec2bin,
    bin2dec,
    bin2decBy,
    insUpd,
    debug,
    (|>),
    (?),
    Cond (..),
    manhattan2d, add2d, add3d, perimeter4, perimeter8,
    areaShoelace
  )
where

import qualified Data.Char as Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)

readInt :: String -> Int
readInt = read

applyN n f = foldr (.) id $ replicate n f

replace fromC toC = map (\c -> if c == fromC then toC else c)

freq s = M.fromListWith (+) [(c, 1) | c <- s]

replaceNth pos newVal list = take pos list ++ newVal : drop (pos + 1) list

dec2bin :: Int -> [Char]
dec2bin =
  reverse . map Char.intToDigit
    . unfoldr
      (\x -> if x == 0 then Nothing else Just (rem x 2, div x 2))

bin2dec = sum . map (2 ^) . findIndices (== '1') . reverse

bin2decBy upper = sum . map (2 ^) . findIndices (== upper) . reverse

printMatrix :: [Int] -> [Int] -> S.Set (Int, Int) -> String
printMatrix xs ys store =
    unlines [show [Maybe.fromMaybe '.'
                   $ fmap (const '#')
                   $ S.lookupIndex (x, y) store| x <- xs ]
                | y <- ys]

insUpd store k val = case M.lookup k store of
  Just _ -> M.update (\_ -> Just val) k store
  Nothing -> M.insert k val store

debug s a = trace (s ++ ":" ++ (show a)) a

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l) : (chunksOf n (drop n l))

-- pipe operator
infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

-- ternary operator
data Cond a = a :? a

infixl 0 ?

infixl 1 :?

(?) :: Bool -> Cond a -> a
True ? (x :? _) = x
False ? (_ :? y) = y

-- perimeters, neighbours
manhattan2d (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

add2d (x, y) (dx, dy) = (x + dx, y + dy)

add3d (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)

perimeter4 xy= map (add2d xy) [          (-1, 0),
                               ( 0, -1),          ( 0, 1),
                                         ( 1, 0) ]

perimeter8 xy= map (add2d xy) [(-1, -1), (-1, 0), (-1, 1),
                               ( 0, -1),          ( 0, 1),
                               ( 1, -1), ( 1, 0), ( 1, 1) ]

-- | Area of a polygon using Shoelace formula
-- The coords must close a loop, it means the first and the last must be the same.
areaShoelace ps = [x1 * y2 - x2 * y1 | (x1, y1) : (x2, y2) : _  <- tails ps] |> sum |> abs |> \x -> x `quot` 2
