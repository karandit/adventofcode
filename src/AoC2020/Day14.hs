module AoC2020.Day14 where

import qualified Data.Map as M
import Utils (readInt, bin2dec, dec2bin, insUpd)

data Instr = Mask String | Write (String, String) deriving Show

readInstrs input = 
 let readMemAndValue (mem:_:value:[]) = (pad0 . dec2bin . readInt . init $ mem, pad0 . dec2bin . readInt $ value)
     readInstr ('m':'a':'s':'k':' ':'=':' ':mask) = Mask mask
     readInstr ('m':'e':'m':'[':memAndValue)      = Write $ readMemAndValue $ words memAndValue
 in map readInstr $ lines input

pad0 x =  replicate (36 - length x) '0' ++ x

------
part1 mask store mem val = insUpd store mem $ map(\(m,v) -> if m /= 'X' then m else v) $ zip mask val

genMems mask mem = 
   map (reverse)
   $ foldl (\mems (maskB, memB) -> 
        case maskB of
        '0' -> map (\amem ->  memB:amem) mems
        '1' -> map (\amem -> maskB:amem) mems
        'X' -> (map (\amem -> '0':amem) mems) ++ (map (\amem -> '1':amem) mems)
    ) [""] 
    $ zip mask mem

part2 mask store mem val = foldl (\ss newmem -> insUpd ss newmem val) store $ genMems mask mem

solution f = 
  let foldInstr (_,   store) (Mask mask)        = (mask, store)
      foldInstr (mask,store) (Write (mem, val)) = (mask, f mask store mem val)
  in sum . map bin2dec . M.elems . snd . foldl foldInstr ("", M.empty) .readInstrs

aoc202014a = solution part1
aoc202014b = solution part2
