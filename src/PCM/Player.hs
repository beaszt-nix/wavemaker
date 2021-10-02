{-# LANGUAGE RankNTypes #-}

module PCM.Player where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import           Data.Array.ST
import           GHC.Real

data PlayerState a = PlayerState {
  pitchStandard :: a,
  tone :: Int -> Int -> a,
  tempo :: Integer,
  sampleRate :: Integer,
  volume :: a,
  builder :: a -> B.Builder
}

defaultPlayerState :: PlayerState Float
defaultPlayerState = PlayerState {
  pitchStandard = 440,
  tone = \f n -> sin (2 * pi * fromIntegral n * fromIntegral f / 44100 ),
  tempo = 100,
  sampleRate = 44100,
  volume = 0.35,
  builder = B.floatLE
}

semitone :: Floating a => PlayerState a -> Int -> a
semitone ps n = pitchStandard ps * ((2 ** (1/12)) ** fromIntegral n)

note :: (RealFrac a, Floating a, Enum a) => PlayerState a -> (Integer, Int, Bool) -> B.Builder
note ps (beat, semi, rest) = foldMap (waveEq ps) $ take (numSamples ps beat) [0 ..]
  where
    numSamples ps beat = round $ (60 * sampleRate ps :% tempo ps) / (beat :% 1)

    waveEq ps = builder ps . (*) vol . tone ps (noteMul semi)
    vol = if rest then 0 else volume ps

    noteMul :: Int -> Int
    noteMul n = round $ semitone ps n
