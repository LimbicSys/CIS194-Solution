module Main where

import Buffer
import Editor
import JoinList
import Scrabble
import Sized

-- initialList :: JoinList (Score, Size) String
-- initialList =
--     fromString "This buffer is for notes you don't want to save, and for"
--         +++ fromString "evaluation of steam valve coefficients."
--         +++ fromString "To load a different file, type the character L followed"
--         +++ fromString "by the name of the file."

main =
    runEditor editor initialList
