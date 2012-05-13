-- Romkan with Haskell.

module Romkan where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

{-
see romkan.csv
-}

boin :: Parser Char
boin = oneOf "aiueo"

siinY :: Parser Char
siinY = oneOf "bfghjklmprvz"

siinYH :: Parser Char
siinYH = oneOf "cdsw"

siinYHS :: Parser Char
siinYHS = char 't'

optionY :: Parser Char
optionY = char 'y'

optionYH :: Parser Char
optionYH = oneOf "yh"

optionYHS :: Parser Char
optionYHS = oneOf "yhs"


romaji_char :: Parser Char
romaji_char = ((char 'y' >> oneOf "aieo")
               <|> (char 'q' >> boin)
               <|> (siinY    >> optional optionY   >> boin)
               <|> (siinYH   >> optional optionYH  >> boin)
               <|> (siinYHS  >> optional optionYHS >> boin)
               <|> (char 'x' >> (char 'n' <|> (optional optionY >> boin)))
               <|> (char 'n' >> (char 'n' <|> (optional optionY >> boin) <|> lookAhead (noneOf "aiueoyn")))
               <|> boin)

romaji_str :: Parser String
romaji_str = many romaji_char
