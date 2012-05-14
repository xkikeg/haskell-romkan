-- Romkan with Haskell.

module Romkan where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

{-
see romkan.csv
-}

listHiraKana =
  [
    "あいうえお",
    "かきくけこ",
    "さしすせそ",
    "たちつてと",
    "なにぬねの",
    "はひふへほ",
    "まみむめも",
    "やいゆえよ",
    "らりるれろ",
    "わゐうゑを",
    "がぎぐげご",
    "ざじずぜぞ",
    "だぢづでど",
    "ばびぶべぼ",
    "ぱぴぷぺぽ"
  ]


data Vowel = VowelA | VowelI | VowelU | VowelE | VowelO


getVowelOrder x =
  case x of
    VowelA -> 0
    VowelI -> 1
    VowelU -> 2
    VowelE -> 3
    VowelO -> 4


data Consonant =
  ConsonantNone
  | ConsonantK
  | ConsonantS
  | ConsonantT
  | ConsonantN
  | ConsonantH
  | ConsonantM
  | ConsonantY
  | ConsonantR
  | ConsonantW
  | ConsonantG
  | ConsonantZ
  | ConsonantD
  | ConsonantB
  | ConsonantP
  | ConsonantC -- begin of irregular consonant
  | ConsonantF
  | ConsonantJ
  | ConsonantL -- not consonant?
  | ConsonantQ
  | ConsonantV
  | ConsonantX


getConsonantOrder :: Consonant -> Int
getConsonantOrder x =
  case x of
    ConsonantNone -> 0
    ConsonantK -> 1
    ConsonantS -> 2
    ConsonantT -> 3
    ConsonantN -> 4
    ConsonantH -> 5
    ConsonantM -> 6
    ConsonantY -> 7
    ConsonantR -> 8
    ConsonantW -> 9
    ConsonantG -> 10
    ConsonantZ -> 11
    ConsonantD -> 12
    ConsonantB -> 13
    ConsonantP -> 14


getHiraKana :: Consonant -> Vowel -> Char
getHiraKana c v = (listHiraKana !! getConsonantOrder c) !! getVowelOrder v

toHiraKanaChars :: Maybe Char -> Consonant -> Vowel -> String
toHiraKanaChars Nothing c v = [getHiraKana c v]


boin :: Parser Char
boin = oneOf "aiueo"

siin :: Parser Char
siin = noneOf "aiueo"

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


romaji_char_wo_xtu :: Parser Char
romaji_char_wo_xtu =
  (char 'y' >> oneOf "aieo")
  <|> (char 'q' >> boin)
  <|> (siinY    >> optional optionY   >> boin)
  <|> (siinYH   >> optional optionYH  >> boin)
  <|> (siinYHS  >> optional optionYHS >> boin)
  <|> (char 'x' >> (char 'n' <|> (optional optionY >> boin)))
  <|> (char 'n' >> (char 'n' <|> (eof >> return 'n')
                    <|> (lookAhead (noneOf "aiueoyn") >> return 'n')
                    <|> (optional optionY >> boin)))
  <|> boin


romaji_char :: Parser Char
romaji_char =
  try (do
          c <- siin
          lookAhead $ char c
          romaji_char_wo_xtu)
  <|> romaji_char_wo_xtu


romaji_str :: Parser String
romaji_str = many romaji_char
