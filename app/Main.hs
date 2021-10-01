{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad
import           Data.Array.MArray
import           GHC.Real
import           PCM.Player
import           PCM.Wave
import           Data.Monoid(mconcat)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Binary.Put(runPut)
import qualified Sample.Discrete.Input      as D
import qualified Sample.Bezier.Tone         as Bez
import           Control.Monad.Trans.Reader
import           System.Environment
import           Data.Maybe(fromMaybe)
import           Parser.Logic
import           Text.Parsec

getTempo ["-t", x] = Just (read x)
getTempo _ = Nothing

getFileName ["-i", x] = Just x
getFileName _ = Nothing

getArgs' :: ([String] -> Maybe a) -> [String] -> IO (Maybe a,[String])
getArgs' f xs = return $ 
  case f (take 2 xs) of
    x@(Just _)  -> (x, drop 2 xs)
    Nothing     -> (Nothing, xs)

main :: IO ()
main = do
  !f <- Bez.mkTone 16000 600 400
  getArgs >>= print
  (tempo', xs)     <- getArgs >>= getArgs' getTempo
  (fileName, xs')  <- getArgs' getFileName xs

  let ps  = PlayerState {
          pitchStandard = 440,
          tone = f,
          tempo = fromMaybe 100 tempo',
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

  case fileName of 
    Just name -> do
      phrases <- words <$> readFile name
      let mus    = either (error "Invalid Sequence") id $ mapM (parse parseNote "") phrases
          !music = mconcat $ map (note ps . mapNote) mus
          res    = B.toLazyByteString music
          wav    = mkWav wv {dataChunk = BL.toStrict res}
      BL.writeFile "output.wav" $ runPut wav
    Nothing -> do
      let 
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
