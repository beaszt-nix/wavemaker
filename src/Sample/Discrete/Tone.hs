module Sample.Discrete.Tone (
  mkTone
) where


import Sample.Discrete.Types
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Array.MArray
import Data.Array
import GHC.Real
import Data.Int

mkTone :: (Floating a) => ToneT (Int -> Int -> a)
mkTone = do 
  ting <- ask 
  arr <- liftIO $ freeze ting :: ToneT (Array Int32 (Ratio Int32))
  return $ 
    \n f ->
      let 
        len      = ((+) 1 . snd . bounds) arr
        k        = mod (fromIntegral n * fromIntegral f) len
        (a :% b) = arr ! k 
        in fromIntegral a / fromIntegral b
