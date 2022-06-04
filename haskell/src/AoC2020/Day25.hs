{-# LANGUAGE BangPatterns #-}

module AoC2020.Day25
  ( aoc202025,
  )
where

import Utils (readInt, (|>))

aoc202025 input =
  let [cardPublicKey, doorPublicKey] = input |> lines |> map readInt
      cardLoopSize = repeatTrans 7 cardPublicKey True
      --doorLoopSize = repeatTrans 7 doorPublicKey True
      encryptionKey = repeatTrans doorPublicKey cardLoopSize False
   in --encryptionKeySame = repeatTrans cardPublicKey doorLoopSize False
      encryptionKey

repeatTrans privateKey publicKey isLoopSize = go 1 0
  where
    go value loopSize =
      let chkValue = if isLoopSize then value else loopSize
          retValue = if isLoopSize then loopSize else value
       in if chkValue == publicKey
            then retValue
            else
              let !value' = (privateKey * value) `mod` 20201227
                  !loopSize' = loopSize + 1
               in go value' loopSize'
