{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.Array.MArray
import GHC.Real
import PCM.Player
import PCM.Wave
import Data.Monoid(mconcat)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put(runPut)
import qualified Sample.Discrete.Input as D
import qualified Sample.Bezier.Tone as Bez
import Control.Monad.Trans.Reader

main :: IO ()
main = do
  !f <- Bez.mkTone 16000 600 400
  let 
    --ps = defaultPlayerState
    ps = PlayerState {
      pitchStandard = 440,
      tone = f,
      tempo = 100,
      sampleRate = 16000,
      volume=0.01,
      builder=B.floatLE 
    }
    wv = Wav {
      dataChunk =  B.empty ,
      wavSampleRate = fromIntegral $ sampleRate ps,
      channels = 1,
      bitsPerSample = 32
    }
    !notes = mconcat [
        note ps (1,-8 + 0, False)
      , note ps (1,-8 + 1, False)
      , note ps (1,-8 + 2, False)
      , note ps (1,-8 + 3, False)
      , note ps (1,-8 + 4, False)
      , note ps (1,-8 + 5, False)
      , note ps (1,-8 + 6, False)
      , note ps (1,-8 + 7, False)
      , note ps (1,-8 + 8, False)
      , note ps (1,-8 + 9, False)
      , note ps (1,-8 + 10, False)
      , note ps (1,-8 + 11, False)
      , note ps (1,-8 + 12, False)]
    res   = B.toLazyByteString notes
    wav   = mkWav wv {dataChunk = BL.toStrict res}
  BL.writeFile "output.wav" $ runPut wav
