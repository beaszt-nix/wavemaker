module Sample.Discrete.Tone
  ( mkTone
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Array
import           Data.Array.MArray
import           Data.Int
import           GHC.Real
import           Sample.Discrete.Types

mkTone :: (Floating a) => ToneT (Int -> Int -> a)
mkTone = do
  ting <- ask
  arr <- liftIO $ freeze ting :: ToneT (Array Int32 (Ratio Int32))
  return $ \n f ->
    let len = ((+) 1 . snd . bounds) arr
        k = mod (fromIntegral n * fromIntegral f) len
        (a :% b) = arr ! k
     in fromIntegral a / fromIntegral b
