module AoC2022.Day25
( aoc202225
) where

import Utils ((|>))
import Data.List (foldl')

parse '=' = -2
parse '-' = -1
parse '0' = 0
parse '1' = 1
parse '2' = 2

-- | Converts from SNAFU to decimal
--
-- >>> map fromSNAFU ["1","2","1=","1-","10","11","12","2=","2-","20"]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> map fromSNAFU ["1=0","1-0","1=11-2","1-0---0","1121-1110-1=0"]
-- [15,20,2022,12345,314159265]
fromSNAFU snafu = foldl' (\acc c -> acc * 5 + parse c) 0 snafu

toChar 0 = '0'
toChar 1 = '1'
toChar 2 = '2'
toChar 3 = '='
toChar 4 = '-'

-- | Converts from decimal to SNAFU
--
-- >>> map toSNAFU [1..10]
-- ["1","2","1=","1-","10","11","12","2=","2-","20"]
--
-- >>> map toSNAFU [15,20,2022,12345,314159265]
-- ["1=0","1-0","1=11-2","1-0---0","1121-1110-1=0"]
toSNAFU n =
    let go acc 0 = acc
        go acc n = go ((n `mod` 5):acc) (n `div` 5)
    in go [] n
        |> reverse
        |> foldl' (\(carry, bits) i ->
            let i' = i + carry
            in (if i' > 2 then 1 else 0, (i' `mod` 5):bits)) (0, [])
        |> (\(carry, bits) -> if carry == 1 then 1:bits else bits)
        |> map toChar

aoc202225 input = input |> lines |> map fromSNAFU |> sum |> toSNAFU
