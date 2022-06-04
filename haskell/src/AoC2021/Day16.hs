{-# LANGUAGE ScopedTypeVariables #-}

module AoC2021.Day16
  ( aoc202116,
  )
where

import Numeric (readHex)
import Text.Printf (printf)
import Utils (bin2dec, readInt, (|>))

hex2bin :: Char -> String
hex2bin c = [c] |> readHex |> head |> fst |> \(n :: Int) -> printf "%04b" n

data Packet = Lit Integer Integer Integer | Op Integer Integer [Packet] deriving (Show)

aoc202116 input = (part1, part2)
  where
    inputHex = input |> lines |> head |> map hex2bin |> concat

    decodeLit :: Integer -> Integer -> String -> (String, [Packet])
    decodeLit ver typ bits =
      let (resBits, binbits) =
            (bits, "")
              |> until
                (\(b1 : _, _) -> b1 == '0')
                (\(bs, accBinBits) -> bs |> splitAt 5 |> \(_ : b4, rest) -> (rest, accBinBits ++ b4))
          (resultBits, binaryBits) = resBits |> splitAt 5 |> \(_ : b4, rest) -> (rest, binbits ++ b4)
          literal = binaryBits |> bin2dec
       in (resultBits, [Lit ver typ literal])

    decodeOp :: Integer -> Integer -> String -> (String, [Packet])
    decodeOp ver typ (b1 : bits) =
      let lengthTyp = (if b1 == '0' then 15 else 11)
          lengthBits = bits |> take lengthTyp |> bin2dec |> fromIntegral
          bitsPackets = bits |> drop lengthTyp
          (restbits, subpackets) = case lengthTyp of
            15 -> (bitsPackets |> drop lengthBits, bitsPackets |> take lengthBits |> decode |> snd)
            11 ->
              let (restBs, _, restPcs) =
                    (bitsPackets, lengthBits, [])
                      |> until
                        (\(_, counter, _) -> counter == 0)
                        ( \(bitsToProc, counter, accPackets) ->
                            let (bitsRest, packets) = decodePacket bitsToProc
                             in (bitsRest, counter - 1, accPackets ++ packets)
                        )
               in (restBs, restPcs)
       in (restbits, [Op ver typ subpackets])

    decodePacket :: String -> (String, [Packet])
    decodePacket (v1 : v2 : v3 : t1 : t2 : t3 : bits) =
      let (ver, typ) = ([v1, v2, v3] |> bin2dec, [t1, t2, t3] |> bin2dec)
       in case typ of
            4 -> bits |> decodeLit ver typ
            _ -> bits |> decodeOp ver typ

    decode :: String -> (String, [Packet])
    decode bits =
      (bits, [])
        |> until
          (\(bs, _) -> (length bs) < 8)
          (\(bs, packets) -> let (newXs, newPackets) = decodePacket bs in (newXs, packets ++ newPackets))

    (_, packets) = decode inputHex

    verz (Lit v _ _) = v
    verz (Op v _ ps) = v + (ps |> map verz |> sum)
    part1 = packets |> map verz |> sum

    val (Lit v 4 x) = x
    val (Op v 0 ps) = ps |> map val |> sum
    val (Op v 1 ps) = ps |> map val |> product
    val (Op v 2 ps) = ps |> map val |> minimum
    val (Op v 3 ps) = ps |> map val |> maximum
    val (Op v 5 [p1, p2]) = if val p1 > val p2 then 1 else 0
    val (Op v 6 [p1, p2]) = if val p1 < val p2 then 1 else 0
    val (Op v 7 [p1, p2]) = if val p1 == val p2 then 1 else 0
    part2 = packets |> map val |> sum
