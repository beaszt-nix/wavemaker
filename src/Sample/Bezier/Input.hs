{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sample.Bezier.Input (
  getSamples,
  newState,
  deCasteljau
) where

import Data.Array.IO
import Control.Monad.Trans.State
import Data.Int
import SDL
import SDL.Vect(V2(..), Point(..), V2(..))
import Control.Monad.IO.Class(liftIO)
import Control.Monad
import GHC.Real(Ratio(..))
import Sample.Bezier.Types
import qualified Data.Vector as V

newState :: Integer -> Integer-> IO DrawState
newState ht wt = return DT {
  height = ht,
  width = wt,
  samples = V.empty
}

getSamples :: DrawState -> IO Sample
getSamples ds = do
  window <- createWindow "Draw Timbre Shape" defaultWindow { 
    windowInitialSize = V2 (fromIntegral $ width ds ) (fromIntegral $ height ds),
    windowHighDPI = True
  }
  renderer <- createRenderer window (-1) defaultRenderer
  samples <$> execStateT (renderLoop renderer) ds


deCasteljau :: RealFrac a => a -> [Point V2 a] -> Point V2 a
deCasteljau t [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where
    reduced = zipWith (lerpP t) coefs (tail coefs)
    lerpP t (P (V2 x0 y0)) (P (V2 x1 y1)) = P (V2 (lerp t x0 x1) (lerp t y0 y1))
    lerp t a b = t * b + (1 - t) * a

bezierPoint :: (RealFrac a) => a -> DrawT (Point V2 Int32)
bezierPoint t = do
  controlPoints <- Control.Monad.Trans.State.gets (V.toList . V.map (fromIntegral <$>) . samples)
  let res = round <$> deCasteljau t controlPoints
  return res

getPosition :: Event -> Maybe (Point V2 Int32)
getPosition (Event _ (MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ pos))) = return pos
getPosition _ = Nothing

renderLoop :: Renderer -> DrawT ()
renderLoop renderer = do
  event <- pollEvent
  controlPoints <- map (fromIntegral <$> ) <$> Control.Monad.Trans.State.gets (V.toList . samples)
  ht <- Control.Monad.Trans.State.gets height
  wt <- Control.Monad.Trans.State.gets width
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 0 0 255 255
  drawLine renderer (P (V2 0 (div (fromIntegral ht) 4))) (P (V2 (fromIntegral wt) (div (fromIntegral ht) 4)))
  drawLine renderer (P (V2 0 (div (3 * fromIntegral ht) 4))) (P (V2 (fromIntegral wt) (div (3 * fromIntegral ht) 4)))

  rendererDrawColor renderer $= V4 0 255 0 255
  drawLine renderer (P (V2 0 (div (fromIntegral ht) 2))) (P (V2 (fromIntegral wt) (div (fromIntegral ht) 2)))

  rendererDrawColor renderer $= V4 0 255 0 255
  mapM_ (drawPoint renderer) controlPoints

  rendererDrawColor renderer $= V4 255 0 0 255
  vec <- Control.Monad.Trans.State.gets (V.length . samples)
  when (vec /= 0) $ do
    forM [0.0,0.01..1] bezierPoint
      >>= mapM_ (drawPoint renderer . (fromIntegral <$>))

  present renderer
  maybe (renderLoop renderer) (eventProcess renderer) event
  where
    eventProcess :: Renderer -> Event -> DrawT ()
    eventProcess r (Event _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeQ _)))) = return ()
    eventProcess r (Event _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeR _)))) = do
      Control.Monad.Trans.State.modify ( \x -> x { samples = V.init $ samples x })
      renderLoop r
    eventProcess r (Event _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeC _)))) = do
      Control.Monad.Trans.State.modify ( \x -> x { samples = V.empty } )
      renderLoop r
    eventProcess r e = do
      case getPosition e of
        Just pos -> do
          Control.Monad.Trans.State.modify (\x -> x { samples = V.snoc (samples x) pos })
        Nothing -> return ()
      renderLoop r
