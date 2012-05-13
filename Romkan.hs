-- Romkan with Haskell.

module Romkan where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

boin :: Parser Char
boin = oneOf "aiueo"

siin1 :: Parser Char
siin1 = oneOf "kstnhmrgzdbpv"

-- yagyou = char "y" >> oneOf "auo"
-- 
-- siin1w = char "w" >> oneOf "aieo"
-- 
-- mojin = char "nn" <|> char "n'" <|> char "xn"

romaji_char :: Parser Char
romaji_char = optional siin1 >> boin

romaji_str :: Parser String
romaji_str = many romaji_char
