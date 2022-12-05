module AoC2022.Day05
  ( aoc202205,
  )
where

import Data.List (intersect, isInfixOf)
import Data.List.Utils (split)
import Utils (readInt, (|>))

aoc202205 input = (part1, part2)
  where
    inputs = input |> lines
    (leftOver, stackDrawings) =
      until
        (\(ls, _) -> ls |> head |> elem '[' |> not)
        (\(ls, rs) -> (tail ls, head ls : rs))
        (inputs, [])
    stacksNr = head inputs |> length |> (+) 1 |> (\x -> div x 4)
    emptyStacks = repeat " " |> take stacksNr
    stacks =
      foldl
        (\stacks' stackDrawing ->
            stacks'
              |> zip [0 ..]
              |> map
                ( \(i, stack) ->
                    let c = stackDrawing !! (i * 4 + 1)
                     in if c == ' ' then stack else c : stack
                )
        ) emptyStacks stackDrawings
    moves =
      leftOver
        |> drop 2
        |> map (split " ")
        |> map (\[_, nr, _, src, _, dst] -> (readInt nr, (readInt src) - 1, (readInt dst) - 1))
    solve f =
      foldl
        ( \stacks' (nr, src, dst) ->
            let (newSrcStack, newDstStack) = f nr (stacks' !! src) (stacks' !! dst)
             in stacks'
                  |> zip [0 ..]
                  |> map
                    ( \(i, stack) ->
                        if i == src
                          then newSrcStack
                          else
                            if i == dst
                              then newDstStack
                              else stack
                    )
        ) stacks moves
        |> map head
    part1 = solve (\nr srcStack dstStack -> (srcStack |> drop nr, (srcStack |> take nr |> reverse) ++ dstStack))
    part2 = solve (\nr srcStack dstStack -> (srcStack |> drop nr, (srcStack |> take nr) ++ dstStack))
