{-# LANGUAGE LambdaCase #-}

module AoC2021.Day18
  ( aoc202118,
  )
where

import Data.List (tails)
import Utils (readInt, (|>))

data SNota = SNr Int | SPair SNota SNota

instance Show SNota where
  show = \case
    SNr n -> show n
    SPair l r -> "[" ++ show l ++ "," ++ show r ++ "]"

aoc202118 input = (part1, part2)
  where
    inputs = input |> lines |> map parseSN
    parseSN s = let ([sn], "") = parse_ [] s in sn

    parse_ acc = \case
      "" -> (acc, "")
      '[' : xs -> let ([le, re], xs') = parse_ [] xs in parse_ (acc ++ [SPair le re]) xs'
      ',' : xs -> parse_ acc xs
      ']' : xs -> (acc, xs)
      d : xs -> parse_ (acc ++ [SNr (readInt [d])]) xs

    reduce :: SNota -> SNota
    reduce sn = case explode 0 sn of
      Just (_, sn', _) -> reduce sn'
      Nothing -> case split sn of
        Just sn' -> reduce sn'
        Nothing -> sn

    explode :: Int -> SNota -> Maybe (Maybe Int, SNota, Maybe Int)
    explode lvl (SNr _) = Nothing
    explode lvl (SPair (SNr l) (SNr r)) = if lvl >= 4 then Just (Just l, SNr 0, Just r) else Nothing
    explode lvl (SPair sl sr) = case explode (lvl + 1) sl of
      Just (jl, sl', jr) -> Just (jl, SPair sl' (addR jr sr), Nothing)
      Nothing -> case explode (lvl + 1) sr of
        Just (jl, sr', jr) -> Just (Nothing, SPair (addL jl sl) sr', jr)
        Nothing -> Nothing

    addR :: Maybe Int -> SNota -> SNota
    addR Nothing sn = sn
    addR (Just l) (SNr r) = SNr (l + r)
    addR (Just l) (SPair sl sr) = SPair (addR (Just l) sl) sr

    addL :: Maybe Int -> SNota -> SNota
    addL Nothing sn = sn
    addL (Just r) (SNr l) = SNr (r + l)
    addL (Just r) (SPair sl sr) = SPair sl (addL (Just r) sr)

    split :: SNota -> Maybe SNota
    split = \case
      sn@(SNr n) -> if n > 9 then Just (SPair ((fromIntegral n / 2) |> floor |> SNr) ((fromIntegral n / 2) |> ceiling |> SNr)) else Nothing
      SPair l r -> case split l of
        Just l' -> Just (SPair l' r)
        Nothing -> case split r of
          Just r' -> Just (SPair l r')
          Nothing -> Nothing

    magnitude (SNr n) = n
    magnitude (SPair l r) = 3 * magnitude l + 2 * magnitude r

    part1 = inputs |> foldl1 (\acc i -> reduce (SPair acc i)) |> magnitude
    part2 =
      inputs
        |> tails
        |> reverse
        |> drop 2
        |> map (\txs -> zip (repeat $ head txs) (tail txs))
        |> concat
        |> map
          ( \(sn1, sn2) ->
              let sn12 = SPair sn1 sn2 |> reduce |> magnitude
                  sn21 = SPair sn2 sn1 |> reduce |> magnitude
               in max sn12 sn21
          )
        |> maximum
