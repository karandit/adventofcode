module AoC2022.Day11
  ( aoc202211,
  )
where

import Data.List (sort, stripPrefix)
import Data.List.Utils (split)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

data Monkey = Monkey
    { insps :: Int
    , items :: [Int]
    , op :: Int -> Int
    , divisor :: Int
    , trueId :: Int
    , falseId :: Int
    }

aoc202211 input = (part1, part2)
  where
    dummyMonkey = Monkey 0 [] id 0 0 0

    parseOp [v1, op, v2] =
      ( \o ->
          let c1 = if v1 == "old" then o else readInt v1
              oper = if op == "*" then (*) else (+)
              c2 = if v2 == "old" then o else readInt v2
           in oper c1 c2
      )

    parseMonkey [_, l1, l2, l3, l4, l5] = Monkey
      { insps   = 0,
        items   = l1 |> stripPrefix "  Starting items: " |> Maybe.fromMaybe "" |> split ", " |> map readInt,
        op      = l2 |> stripPrefix "  Operation: new = " |> Maybe.fromMaybe "" |> split " " |> parseOp,
        divisor = l3 |> stripPrefix "  Test: divisible by " |> Maybe.fromMaybe "" |> readInt,
        trueId  = l4 |> stripPrefix "    If true: throw to monkey " |> Maybe.fromMaybe "" |> readInt,
        falseId = l5 |> stripPrefix "    If false: throw to monkey " |> Maybe.fromMaybe "" |> readInt
      }

    inputs = input |> lines |> split [""] |> map parseMonkey

    processRound reducer monkeys i =
      let Monkey _ itemList f divisor trueId falseId = monkeys |> M.lookup i |> Maybe.fromMaybe dummyMonkey

          processItem acc2 item =
            let item2 = item |> f |> reducer
                targetId = if item2 `mod` divisor == 0 then trueId else falseId
             in acc2 |> M.adjust (\foundMonkey -> foundMonkey { items = items foundMonkey ++ [item2] }) targetId

          acc' = itemList |> foldl processItem monkeys
       in acc' |> M.adjust (\foundMonkey -> foundMonkey { insps = insps foundMonkey + length itemList, items = []}) i

    solve roundCnt reducer =
       let monkeysById = inputs |> zip [0 ..] |> M.fromList
           rounds = monkeysById  |> iterate (\monkeys -> [0 .. M.size monkeys -1] |> foldl (processRound reducer) monkeys)
           lastRound = rounds |> take (roundCnt + 1) |> last
           inspections = lastRound |> M.elems |> map insps
        in inspections |> sort |> reverse |> take 2 |> product

    part1 = solve 20 (`div` 3)

    dv = inputs |> map divisor |> product
    part2 = solve 10000 (`mod` dv)
