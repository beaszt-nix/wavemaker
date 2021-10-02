{-# LANGUAGE BangPatterns #-}

module Sample.Bezier.Tone
  ( mkTone
  ) where

import qualified Data.Vector         as V
import           SDL.Vect
import           Sample.Bezier.Input
import           Sample.Bezier.Types

mkTone :: RealFrac a => Integer -> Integer -> Integer -> IO (Int -> Int -> a)
mkTone sampleRate wt ht = do
  sample <- newState ht wt >>= getSamples
  let !vec = V.toList . V.map (fromIntegral <$>) $ sample
  return $ \n f ->
    let t = mod (n * f) (fromIntegral sampleRate)
        i = fromIntegral t / fromIntegral sampleRate
        (P (V2 a b)) = deCasteljau i vec
     in 2 * (fromIntegral ht - 2 * b) / fromIntegral ht
