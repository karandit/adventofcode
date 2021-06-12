module AoC2020.Day21
( aoc202021a
, aoc202021b
) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (sortBy, groupBy, nub, intercalate)
import Data.List.Utils (split)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Maybe (maybe)
import Utils ((|>))

type Ingredient = String
type Allergen = String
type Food = ([Ingredient], [Allergen])
type Finding = (Allergen, Ingredient)
type AllergsIngreds = M.Map Allergen (S.Set Ingredient)
type IngredsAllergs = M.Map Ingredient (S.Set Allergen)

parseFood :: String -> Food
parseFood s =
    let p1:p2:[] = s |> split " (contains "
        ingredients = p1 |> words
        allergens = p2 |> init |> split ", "
    in (ingredients, allergens)

groupFoodsBy :: (Ord k, Ord v) => (Food -> [k]) -> (Food -> [v]) -> (S.Set v -> S.Set v -> S.Set v) -> [Food] -> M.Map k (S.Set v)
groupFoodsBy getKeys getValues sOp foods =
                [(key, getValues food) | food <- foods, key <- getKeys food]
                |> sortBy (compare `on` fst)
                |> groupBy (\(x1, _) (x2, _) -> x1 == x2)
                |> map (\xs -> (xs |> head |> fst, xs |> map (S.fromList . snd) |> foldl1 sOp))
                |> M.fromList

solve :: AllergsIngreds -> IngredsAllergs -> [Finding] -> [Finding] -> [Finding]
solve allergsIngreds ingredsAllergs acc [] = acc
solve allergsIngreds ingredsAllergs acc findings =
    let 
        (newAllergsIngreds, newFindings) =
            foldl' (\acc1 (_, ingr) -> M.lookup ingr ingredsAllergs |> maybe acc1 (\allergs ->
                foldl' (\acc2@(aByName, sA) a -> M.lookup a aByName |> maybe acc2 (\ingrs ->
                    let ingrs' = S.delete ingr ingrs
                    in (M.adjust (\_ -> ingrs') a aByName, if S.size ingrs' == 1 then (a, S.elemAt 0 ingrs'):sA else sA)
                )) acc1 allergs
            )) (allergsIngreds, []) findings
    in solve newAllergsIngreds ingredsAllergs (acc ++ findings) newFindings

getFoodsAndFindings :: String -> ([Food], [Finding])
getFoodsAndFindings input =
  let
     foods = input |> lines |> map parseFood
     allergsIngreds = foods |> groupFoodsBy snd fst S.intersection
     ingredsAllergs = foods |> groupFoodsBy fst snd S.union
     firstFindings = allergsIngreds |> M.filter (\ingrs -> 1 == (S.size ingrs)) |> M.map (head . S.elems) |> M.toList
     findings = solve allergsIngreds ingredsAllergs [] firstFindings
  in (foods, findings)

aoc202021a input =
  let (foods, findings) = input |> getFoodsAndFindings
      ingrNoAllerg = findings |> map snd |> S.fromList
  in foods |> map (\food -> S.difference (food |> fst |> S.fromList) ingrNoAllerg |> S.size) |> sum

aoc202021b input = input |> getFoodsAndFindings |> snd |> sortBy (compare `on` fst) |> map snd |> nub |> intercalate ","
