module Sample.Discrete.Types
  ( Sample(..)
  , DrawState(..)
  , DrawT(..)
  , ToneT(..)
  ) where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Array.IO
import           Data.Int
import           Data.Ratio
import           Foreign.C.Types
import           SDL.Vect

type Sample = IOArray Int32 (Ratio Int32)

data DrawState =
  DT
    { samples :: Sample
    , height  :: Integer
    , width   :: Integer
    }

type DrawT = StateT DrawState IO

type ToneT = ReaderT Sample IO
