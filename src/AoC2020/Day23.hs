module AoC2020.Day23
( aoc202023
) where

import           Control.Monad.Primitive        (PrimMonad, PrimState)
import           Control.Monad.ST               (ST, runST)
import           Data.Char                      (digitToInt)
import           Data.Foldable                  (for_)
import           Data.Vector.Unboxed            (Vector)
import qualified Data.Vector.Unboxed            as V
import           Data.Vector.Unboxed.Mutable    (STVector)
import qualified Data.Vector.Unboxed.Mutable    as MV
import           Utils                          ((|>), readInt)

destCup threshold x1 pickeds =
    let x = if x1 == 0 then threshold else x1-1
    in if not $ elem x pickeds
       then x
       else destCup threshold x pickeds

initCups ::
         Int -> [Int] -> ST s (STVector s Int)
--initCups :: (PrimMonad m)
--        => Int -> [Int] -> m (MVector (PrimState m) Int)
initCups n nrs = do
    v <- MV.new n
    for_ [0 .. n-2] $ \i -> MV.unsafeWrite v i (i+1)
    MV.unsafeWrite v (n-1) (head nrs)
    let v0 = nrs |> V.fromList
    let n0 = V.length v0
    for_ [0 .. n0-2] $ \i -> MV.unsafeWrite v (v0 V.! i) (v0 V.! (i+1))
    pure v

moveCups ::
         Int -> Int -> Int -> STVector s Int
        -> ST s ()
--moveCups :: (PrimMonad m)
--        => Int -> Int -> Int -> MVector (PrimState m) Int
--        -> m ()
moveCups 0 _ _ _ = pure ()
moveCups cnt threshold i v = do
        i1 <- MV.unsafeRead v i
        i2 <- MV.unsafeRead v i1
        i3 <- MV.unsafeRead v i2
        n1 <- MV.unsafeRead v i3
        MV.unsafeWrite v i n1
        let d = destCup threshold i [i1, i2, i3]
        d1 <- MV.unsafeRead v d
        MV.unsafeWrite v d i1
        MV.unsafeWrite v i3 d1
        moveCups (cnt-1) threshold n1 v

--solve :: (PrimMonad m, PrimState m ~ s)
solve ::
          ([Int] -> Int)
         -> ([Int] -> Int)
         -> Int
         -- -> (MVector (PrimState m) Int -> m String)
         -- -> (STVector s Int -> ST s String)
         -- -> (STVector s Int -> ST s String)
         -> Bool
         -> String
         -> String
--solve linkLastTo nrOfCups iterations getAnswer isPart1 input =
solve linkLastTo nrOfCups iterations isPart1 input =
        let nrs = input |> map digitToInt |> map ((+) (-1))
            rightNeigh = nrs ++ [linkLastTo nrs]
            cupsSize = nrOfCups nrs
            getAnswerPart1 :: STVector s Int -> ST s String
            getAnswerPart1 v = do
                    frozen <- V.freeze v
                    let answer = backToCanonical 0 frozen [1]
                    pure $ concat $ map show $ tail $ reverse answer
            getAnswerPart2 :: STVector s Int -> ST s String
            getAnswerPart2 v = do
                    x1 <- MV.read v 0
                    x2 <- MV.read v x1
                    pure $ show $ (x1+1) * (x2+1)
        in runST $ do
            v <- initCups cupsSize rightNeigh
            moveCups iterations (cupsSize-1) (head nrs) v
            --answer <- getAnswer v
            let getAns = if isPart1 then getAnswerPart1 else getAnswerPart2
            answer <- getAns v
            pure answer

backToCanonical :: Int -> Vector Int -> [Int] -> [Int]
backToCanonical i v acc =
        let i1 = v V.! i
        in if i1 == 0 then acc else backToCanonical i1 v ((i1+1):acc)

--getAnswerPart1 :: (PrimMonad m)
--        => MVector (PrimState m) Int -> m String
--getAnswerPart1 ::
        --STVector s Int -> ST s String
--getAnswerPart1 v = do
        --frozen <- V.freeze v
        --let answer = backToCanonical 0 frozen [1]
        --pure $ concat $ map show $ tail $ reverse answer

--getAnswerPart2 :: (PrimMonad m)
--        => MVector (PrimState m) Int -> m String
--getAnswerPart2 ::
        --STVector s Int -> ST s String
--getAnswerPart2 v = do
        --x1 <- MV.read v 0
        --x2 <- MV.read v x1
        --pure $ show $ (x1+1) * (x2+1)

aoc202023 :: String -> (String, String)
aoc202023 input = (part1, part2) where
  part1 = solve (\nrs -> head nrs) (\nrs -> length nrs) 100 True input
  part2 = solve (\_   -> 9)        (\_   -> 1000000)    10000000 False input
