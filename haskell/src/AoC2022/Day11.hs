module AoC2022.Day11
  ( aoc202211,
  )
where

import Data.List (sort, stripPrefix)
import Data.List.Utils (split)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Utils (readInt, (|>))

aoc202211 input = (part1, part2)
  where
    dummyMonkey = (0, [], id, 0, 0, 0)

    parseOp [v1, op, v2] =
      ( \o ->
          let c1 = if v1 == "old" then o else readInt v1
              oper = if op == "*" then (*) else (+)
              c2 = if v2 == "old" then o else readInt v2
           in oper c1 c2
      )

    parseMonkey [_, l1, l2, l3, l4, l5] =
      ( 0,
        l1 |> stripPrefix "  Starting items: " |> Maybe.fromMaybe "" |> split ", " |> map readInt,
        l2 |> stripPrefix "  Operation: new = " |> Maybe.fromMaybe "" |> split " " |> parseOp,
        l3 |> stripPrefix "  Test: divisible by " |> Maybe.fromMaybe "" |> readInt,
        l4 |> stripPrefix "    If true: throw to monkey " |> Maybe.fromMaybe "" |> readInt,
        l5 |> stripPrefix "    If false: throw to monkey " |> Maybe.fromMaybe "" |> readInt
      )

    inputs = input |> lines |> split [""] |> map parseMonkey
    nr = length inputs
    monkeysById = zip [0 ..] inputs |> M.fromList

    processRound reducer monkeys i =
      let (insps, items, f, divisor, trueId, falseId) = monkeys |> M.lookup i |> Maybe.fromMaybe dummyMonkey

          processItem acc2 item =
            let item2 = item |> f |> reducer
                targetId = if item2 `mod` divisor == 0 then trueId else falseId
             in acc2 |> M.adjust (\(tgins, tgItems, tgF, tgtest, tgt, tgf) -> (tgins, tgItems ++ [item2], tgF, tgtest, tgt, tgf)) targetId

          acc' = items |> foldl processItem monkeys
       in acc' |> M.adjust (\_ -> (insps + length items, [], f, divisor, trueId, falseId)) i

    solve rounds reducer =
      let lastRound =
            monkeysById
              |> iterate (\monkeys -> [0 .. nr -1] |> foldl (processRound reducer) monkeys)
              |> take (rounds + 1)
              |> last
       in lastRound |> M.elems |> map (\(i, _, _, _, _, _) -> i) |> sort |> reverse |> take 2 |> product

    part1 = solve 20 (`div` 3)

    dv = inputs |> map (\(_, _, _, divisor, _, _) -> divisor) |> product
    part2 = solve 10000 (`mod` dv)
