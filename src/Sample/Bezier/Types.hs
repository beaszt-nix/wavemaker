module Sample.Bezier.Types (
  Sample(..),
  DrawState(..),
  DrawT(..),
  ToneT(..)
) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Int
import SDL.Vect(Point(..), V2(..))
import Data.Vector

type Sample = Vector (Point V2 Int32)

data DrawState = DT { 
  samples :: Sample,
  height :: Integer,
  width :: Integer
}

type DrawT = StateT DrawState IO
type ToneT = ReaderT Sample IO

