module Parser.Logic where

import           Data.Char
import           Data.Either
import           Data.Maybe
import           Parser.Types
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.Printf

noteExp :: Note -> Int
noteExp (Note reg r isSharp) =
  let rootval =
        case r of
          C -> 0
          D -> 2
          E -> 4
          F -> 5
          G -> 7
          A -> 9
          B -> 11
          R -> -1000
      isSh =
        if isSharp
          then 1
          else 0
   in 3 + rootval + isSh + (reg - 5) * 12

mapNote :: (Rhythm, Note) -> (Integer, Int, Bool)
mapNote (rh, n) =
  let len' = len rh
      no = noteExp n
   in (len', no, no <= -900)
  where
    len rh =
      case rh of
        Whole     -> 1
        Half      -> 2
        Quarter   -> 4
        Eight     -> 8
        Sixteen   -> 16
        ThirtyTwo -> 32

parseNote :: Parser (Rhythm, Note)
parseNote = do
  note' <- oneOf "ABCDEFGR"
  sharp <- option False $ char '#' >> return True
  reg <- many digit >>= return . regProc
  rh <- oneOf "WHQEST"
  return
    ( fromMaybe Quarter . charToRh $ rh
    , Note reg ((fromJust . charToRoot) note') sharp)
  where
    regProc :: [Char] -> Int
    regProc [] = 0
    regProc xs = read xs
    charToRoot :: Char -> Maybe RootNote
    charToRoot c =
      return $
      case c of
        'A' -> A
        'B' -> B
        'C' -> C
        'D' -> D
        'E' -> E
        'F' -> F
        'G' -> G
        'R' -> R
        _   -> error "Invalid"
    charToRh :: Char -> Maybe Rhythm
    charToRh c =
      return $
      case c of
        'W' -> Whole
        'H' -> Half
        'Q' -> Quarter
        'E' -> Eight
        'S' -> Sixteen
        'T' -> ThirtyTwo
        _   -> error "Invalid"
