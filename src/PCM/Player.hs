{-# LANGUAGE RankNTypes #-}

module PCM.Player where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import           Data.Array.ST
import           Data.Array.Unboxed
import           Control.Monad
import           GHC.Real

data PlayerState a = PlayerState {
  pitchStandard :: a,
  samples :: UArray Int a,
  tempo :: Integer,
  sampleRate :: Integer,
  volume :: a,
  builder :: a -> B.Builder
}

defaultPlayerState :: PlayerState Float
defaultPlayerState = 
  let sample = genWave (\f n -> sin (2 * pi * fromIntegral n * fromIntegral f / 44100 )) 44100
    in PlayerState {
        pitchStandard = 440,
        samples = sample,
        tempo = 100,
        sampleRate = 44100,
        volume = 0.35,
        builder = B.floatLE
    }

genWave :: (Int -> Int -> Float) -> Integer -> UArray Int Float 
genWave f sr = runSTUArray $ do
  let d = fromIntegral sr
  arr    <- newArray (0, d-1) 0
  bounds <- getBounds arr
  forM_ (range bounds) 
    $ \x -> do
      writeArray arr x $ f (fromIntegral x) 1
  return arr

semitone :: Floating a => PlayerState a -> Int -> a
semitone ps n = pitchStandard ps * ((2 ** (1/12)) ** fromIntegral n)

note :: PlayerState Float -> (Integer, Int, Bool) -> B.Builder
note ps (beat, semi, rest) = 
  foldMap (waveEq ps) $ take (numSamples ps beat) [0 ..]
  where
    numSamples ps beat = round $ (60 * sampleRate ps :% tempo ps) / (beat :% 1)
    waveEq ps = builder ps . (*) vol . func . noteMul semi
    func n = samples ps ! n 
    vol = if rest then 0 else volume ps
    noteMul semi n = mod (round (semitone ps semi) * n) (fromIntegral (sampleRate ps))
