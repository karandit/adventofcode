module AoC2024.Day09
( aoc202409
) where

import qualified Data.Map as M
import Data.Foldable (foldl')
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

type FileBlocks = [(Int, (Int, Int))]
type FreeBlocks = [(Int, Int)]
type FreeBlocksMap = M.Map Int Int

aoc202409 input = (part1, part2) where
  diskMap = head $ lines input

  decode :: (Int -> (Int, Int)) -> (FreeBlocks, FileBlocks, Int, Int) -> (Char, Bool) -> (FreeBlocks, FileBlocks, Int, Int)
  decode f (freeBlocks, fileBlocks, blockId, fileId) (v, isFile) =
    let (blockSize, blockCount) = f $ digitToInt v
        mkBlocks block = [(i, block)| i <- [blockId..blockId + blockCount - 1]]
        (freeBlocks', fileBlocks', fileId') = if isFile
          then ([], reverse (mkBlocks (blockSize, fileId)), 1)
          else (mkBlocks blockSize, [], 0)
    in (freeBlocks' ++ freeBlocks, fileBlocks' ++ fileBlocks, blockId + blockSize * blockCount, fileId' + fileId)

  defrag :: Int -> FreeBlocksMap -> FileBlocks -> Int
  defrag acc _          [] = acc
  defrag acc freeBlocksMap ((fileBlockId, (fileBlockSize, fileId)):restFileBlocks) =
    let findNewPlace [] = Nothing
        findNewPlace ((freeBlockId, freeBlockSize):restFreeBlocks) =
          if freeBlockId > fileBlockId
          then Nothing
          else if freeBlockSize < fileBlockSize then findNewPlace restFreeBlocks
               else if freeBlockSize == fileBlockSize then Just (M.delete freeBlockId freeBlocksMap, freeBlockId)
               else Just (M.insert (freeBlockId + fileBlockSize) (freeBlockSize - fileBlockSize) $ M.delete freeBlockId freeBlocksMap, freeBlockId)

        (freeBlocksMap', bId') = fromMaybe (freeBlocksMap, fileBlockId) $ findNewPlace (M.toList freeBlocksMap)
    in defrag (sum [fileId * i | i <- [bId'..bId'+fileBlockSize-1]] + acc) freeBlocksMap' restFileBlocks

  solve f =
    let (freeBlocks, fileBlocks, _, _) = foldl' (decode f) ([], [], 0, 0) $ zip diskMap (cycle [True, False])
    in defrag 0 (M.fromList freeBlocks) fileBlocks

  part1  = solve (\n -> (1, n))
  part2  = solve (\n -> (n, 1))
