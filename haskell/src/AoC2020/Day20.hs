module AoC2020.Day20
  ( aoc202020,
  )
where

import Data.Function (on)
import Data.List (find, group, groupBy, intercalate, nub, sort, sortBy)
import Data.List.Utils (split)
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import Utils (bin2decBy, readInt, (|>))

data TileEdge = Border | Paired Int Bool deriving (Show, Eq)

toEdge 1 _ _ = Border
toEdge 2 fp flipped = Paired fp flipped

strToFP s =
  let v = s |> bin2decBy '#'
      vr = s |> reverse |> bin2decBy '#'
   in if v < vr then (v, False) else (vr, True)

parseTileFrame :: [String] -> (Int, [(Int, Bool)])
parseTileFrame (descr : lines) =
  let tileId = descr |> (\('T' : 'i' : 'l' : 'e' : ' ' : idrest) -> idrest |> init |> readInt)
      north = lines |> head |> strToFP
      east = lines |> map last |> strToFP
      south = lines |> last |> reverse |> strToFP
      west = lines |> map head |> reverse |> strToFP
   in (tileId, [north, east, south, west])

--parseTileImage :: [String] -> (Int, S.Set (Int, Int))
--parseTileImage (descr:image) =
--    let ('T':'i':'l':'e':' ':idrest) = descr
--        tileId = idrest |> init |> readInt
--        xySet = foldl (\acc1 (rIdx, row) -> if rIdx == 0 || rIdx == 9 then acc1 else
--                     foldl (\acc2 (cIdx, c) -> if cIdx == 0 || cIdx == 9 then acc2 else
--                             if c=='#' then S.insert (rIdx-1, cIdx-1) acc2 else acc2
--                     ) acc1 $ zip [0..] row
--                 ) S.empty $ zip [0..] $ image
--    in (tileId, xySet)

--rotateTileImage :: (Int, S.Set (Int, Int)) -> (Int, S.Set (Int, Int))
--rotateTileImage (tileId, tileXYSet) = (tileId, tileXYSet |> S.map (\(x,y) -> (y,x)))

-- True == is border, False == it has a pair tile
isNW (_, [Border, Paired _ _, Paired _ _, Border]) = True
isNW _ = False

isW efp (_, [_, _, _, Paired wfp _]) = efp == wfp
isW _ _ = False

isN sfp (_, [Paired nfp _, _, _, _]) = sfp == nfp
isN _ _ = False

rotateTile (tileId, [n, e, s, w]) = (tileId, [w, n, e, s])

getE (tileId, [_, e, _, _]) = e

getS (tileId, [_, _, s, _]) = s

main :: IO ()
main = do
  input <- readFile "test/input202020_sample.txt"
  --input <- readFile "test/input202020.txt"
  let inputPerTile = input |> lines |> split [""]
      --tileImages = inputPerTile |> map parseTileImage |> M.fromList
      tiles :: [(Int, [(Int, Bool)])]
      tiles = inputPerTile |> map parseTileFrame
      groups =
        tiles
          |> map snd
          |> concat
          |> map fst
          |> sort
          |> group
          |> map (\fingerprints -> (head fingerprints, length fingerprints))
          |> M.fromList
      tileFrames :: [(Int, [TileEdge])]
      tileFrames =
        tiles
          |> map
            ( \(tileid, frame) ->
                ( tileid,
                  frame
                    |> map
                      ( \(fingerprint, flipped) ->
                          M.lookup fingerprint groups
                            |> maybe 0 id
                            |> (\fpc -> toEdge fpc fingerprint flipped)
                      )
                )
            )

      corners =
        tileFrames
          |> filter (\(_tileid, edges) -> edges |> filter ((==) Border) |> length |> (==) 2)
      part1 = corners |> map fst |> product

      -- part2
      tile0 = corners |> head
      -- (rotCnt, cornerNW) = until (\(cnt, tile) -> isNW tile) (\(cnt, tile) -> (cnt+1, rotateTile tile)) (0, tile0)
      cornerNW = until isNW rotateTile tile0
      tileFramesById =
        tileFrames
          |> M.fromList
      tileIdsByFp =
        tiles
          |> map (\(tileId, frame) -> frame |> map (\(fp, _flipped) -> (fp, tileId)))
          |> concat
          |> sortBy (compare `on` fst)
          |> groupBy (\(fp1, _) (fp2, _) -> fp1 == fp2)
          |> map (\groups -> (groups |> head |> fst, groups |> map snd))
          |> M.fromList

      findNextTile getEdge checkPos tile@(tileId, [n, e, s, w]) = case getEdge tile of
        Border -> Nothing
        (Paired fp _) ->
          (M.lookup fp tileIdsByFp)
            >>= (\tileIds -> tileIds |> find (\aTileId -> aTileId /= tileId))
            >>= (\nextTileId -> M.lookup nextTileId tileFramesById |> fmap (\frame -> (nextTileId, frame)))
            |> fmap (\nextTile -> until (checkPos fp) rotateTile nextTile)
      solvePuzzleToEast =
        until
          (\(maybeTile, _, _) -> Maybe.isNothing maybeTile)
          (\(Just tile, cnt, acc) -> (findNextTile getE isW tile, cnt + 1, tile : acc))
          (Just cornerNW, 0, []) -- S.empty)
      solvePuzzleToSouth =
        until
          (\(maybeTile, _, _) -> Maybe.isNothing maybeTile)
          (\(Just tile, cnt, acc) -> (findNextTile getS isN tile, cnt + 1, tile : acc))
          (Just cornerNW, 0, []) -- S.empty)
  putStrLn $ "       input        :"
  putStrLn $ "tiles: "
  putStrLn $ unlines $ map show $ tiles
  putStrLn $ ""
  putStrLn $ "groups: "
  putStrLn $ unlines $ map show $ M.toList groups
  putStrLn $ ""
  putStrLn $ "       part1        :"
  putStrLn $ "       corners        :"
  putStrLn $ unlines $ map show $ corners
  putStrLn $ show $ part1
  putStrLn $ ""
  putStrLn $ "       part2        :"
  putStrLn $ show $ tile0
  putStrLn $ show $ cornerNW
  --putStrLn $ "rotCnt NW: " ++ show rotCnt
  putStrLn $ "solvePuzzleToEast: " ++ show solvePuzzleToEast
  putStrLn $ ""
  putStrLn $ "solvePuzzleToSouth: " ++ show solvePuzzleToSouth

--putStrLn $ "       tilesByFp        :"
--putStrLn $ unlines $ map show $ M.toList tileIdsByFp
--putStrLn $ "       tilesById        :"
--putStrLn $ unlines $ map show $ M.toList tileFramesById
--putStrLn $ "       tileImages        :"
--putStrLn $ unlines $ map show $ M.toList tileImages

aoc202020 input = (part1, "MISSING")
  where
    inputPerTile = input |> lines |> split [""]
    --tileImages = inputPerTile |> map parseTileImage |> M.fromList
    tiles :: [(Int, [(Int, Bool)])]
    tiles = inputPerTile |> map parseTileFrame
    groups =
      tiles
        |> map snd
        |> concat
        |> map fst
        |> sort
        |> group
        |> map (\fingerprints -> (head fingerprints, length fingerprints))
        |> M.fromList
    tileFrames :: [(Int, [TileEdge])]
    tileFrames =
      tiles
        |> map
          ( \(tileid, frame) ->
              ( tileid,
                frame
                  |> map
                    ( \(fingerprint, flipped) ->
                        M.lookup fingerprint groups
                          |> maybe 0 id
                          |> (\fpc -> toEdge fpc fingerprint flipped)
                    )
              )
          )

    corners =
      tileFrames
        |> filter (\(_tileid, edges) -> edges |> filter ((==) Border) |> length |> (==) 2)
    part1 = corners |> map fst |> product
