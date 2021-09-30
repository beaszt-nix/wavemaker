module PCM.Wave where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B

import Data.Binary
import Data.Binary.Put
import Data.Word
import PCM.Player

data Wave = Wav {
  dataChunk :: B.ByteString,
  wavSampleRate :: Int,
  channels :: Int,
  bitsPerSample :: Int
}

mkWav :: Wave -> Put
mkWav wave = do
  let blockalignx8 = channels wave * bitsPerSample wave
      byteratex8 = blockalignx8 * wavSampleRate wave
      dataSize = fromIntegral . B.length . dataChunk $ wave

  -- Chunk 1 of RIFF 
  putWord32be 0x52494646 --RIFF
  putWord32le $ 36 + dataSize
  putWord32be 0x57415645 --WAVE

  -- Chunk 2 of RIFF (Wave) (fmt)
  putWord32be 0x666d7420

  putWord32le 16
  putWord16le 1
  putWord16le . fromIntegral . channels $ wave
  putWord32le . fromIntegral . wavSampleRate $ wave
  putWord32le $ div (fromIntegral byteratex8) 8
  putWord16le $ div (fromIntegral blockalignx8) 8
  putWord16le . fromIntegral . bitsPerSample $ wave

  -- Chunk 3 Data
  putWord32be 0x64617461

  putWord32le dataSize
  putByteString . dataChunk $ wave
