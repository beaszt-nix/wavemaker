module Parser.Types where

data RootNote = A | B | C | D | E | F | G | R deriving Show

data Note = Note Int RootNote Bool deriving Show

data Rhythm = Whole
            | Half
            | Quarter
            | Eight
            | Sixteen
            | ThirtyTwo deriving Show
