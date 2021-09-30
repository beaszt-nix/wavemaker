{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sample.Discrete.Input (
  getSamples
) where

import Data.Array.IO
import Control.Monad.Trans.State
import Data.Int
import SDL
import SDL.Vect(V2(..), Point(..), V2(..))
import Control.Monad.IO.Class(liftIO)
import Control.Monad
import GHC.Real(Ratio(..))
import Sample.Discrete.Types

coordinateToXY :: Point V2 Int32 -> DrawT (Int32, Ratio Int32)
coordinateToXY (P (V2 x y)) = gets (fromIntegral . height)  >>= \h -> return (x, (h - 2*y) :% h )

xyToCoordinate :: (Int32, Ratio Int32) -> DrawT (Point V2 Int32)
xyToCoordinate (x, k) = gets (fromIntegral . height) >>= \h -> return $ P (V2 x (getDecimal ( (h - k*h) / 2)))
  where
    getDecimal :: Ratio Int32 -> Int32
    getDecimal (a :% b) = if b == 1 then a else error "Invalid Value"

newState :: Integer -> Integer -> IO DrawState
newState ht wt = do 
  let ss = replicate (fromInteger wt) 0 :: [Ratio Int32]
  smps <- newListArray (0, fromIntegral wt - 1) ss
  return DT {
    height = ht, 
    width = wt,
    samples = smps
  } 

getSamples :: Integer -> IO Sample
getSamples wt = do
  state <- newState 300 wt
  window <- createWindow "Draw Timbre Shape" defaultWindow { 
    windowInitialSize = V2 (fromIntegral $ width state ) (fromIntegral $ height state),
    windowHighDPI = True
  }
  renderer <- createRenderer window (-1) defaultRenderer
  samples <$> execStateT (renderLoop renderer) state

getPosition :: Event -> Maybe (Point V2 Int32)
getPosition (Event _ (MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ pos))) = return pos
getPosition (Event _ (MouseMotionEvent (MouseMotionEventData _ _ btns p _))) = 
  if ButtonLeft `elem` btns 
    then return p 
    else Nothing
getPosition _ = Nothing

renderLoop :: Renderer -> DrawT ()
renderLoop renderer = do
  event <- pollEvent
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 0 255
  array <- Control.Monad.Trans.State.gets samples >>= (liftIO . getAssocs) >>= mapM xyToCoordinate
  forM_ array $ \x -> drawPoint renderer (fromIntegral <$> x)
  present renderer
  maybe (renderLoop renderer) (eventProcess renderer) event
  where
    eventProcess :: Renderer -> Event -> DrawT ()
    eventProcess r (Event _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeQ _)))) = return ()
    eventProcess r (Event _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeC _)))) = do
      state <- Control.Monad.Trans.State.gets samples
      state' <- liftIO $ mapArray (const 0) state
      Control.Monad.Trans.State.modify ( \x -> x {samples=state'} )
      renderLoop r
    eventProcess r e = do
      case getPosition e of
        Just pos -> do
          (a,b) <- coordinateToXY pos
          state <- Control.Monad.Trans.State.gets samples
          liftIO $ writeArray state a b
          Control.Monad.Trans.State.modify (\x -> x { samples=state})
        Nothing -> return ()
      renderLoop r

