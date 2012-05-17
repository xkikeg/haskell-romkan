-- Romkan with Haskell.

module Romkan where

import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)

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
    "ぱぴぷぺぽ",
    "ぁぃぅぇぉ",
    "ゃぃゅぇょ"
  ]


data Vowel = VowelA | VowelI | VowelU | VowelE | VowelO
  deriving (Show, Read, Enum)


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
  | ConsonantLittleY
  deriving (Show, Read, Enum)


getConsonantOrder :: Consonant -> Int
getConsonantOrder x =
  case x of
    ConsonantL -> 15
    ConsonantX -> 15
    ConsonantLittleY -> 16
    _ -> fromEnum x


vowelIndex :: Vowel -> ([a] -> a)
vowelIndex v = (!! fromEnum v)


getHiraKana :: Consonant -> Vowel -> String
getHiraKana c v =
  case c of
    ConsonantF -> vowelIndex v ["ふぁ", "ふぃ", "ふ", "ふぇ", "ふぉ"]
    ConsonantJ -> vowelIndex v ["じゃ", "じ", "じゅ", "じぇ", "じょ"]
    ConsonantQ -> vowelIndex v ["くぁ", "くぃ", "く", "くぇ", "くぉ"]
    ConsonantV -> vowelIndex v ["ゔぁ", "ゔぃ", "ゔ", "ゔぇ", "ゔぉ"]
    _ -> [vowelIndex v (listHiraKana !! getConsonantOrder c)]

toHiraKanaChars :: Maybe Consonant -> Consonant -> Vowel -> String
toHiraKanaChars Nothing c v = getHiraKana c v

toHiraKanaChars (Just ConsonantS) ConsonantT v =
  ["つぁ", "つぃ", "つ", "つぇ", "つぉ"] !! fromEnum v

toHiraKanaChars (Just ConsonantH) c v =
  case c of
    ConsonantC -> vowelIndex v ["ちゃ", "ち", "ちゅ", "ちぇ", "ちょ"]
    ConsonantS -> vowelIndex v ["しゃ", "し", "しゅ", "しぇ", "しょ"]
    ConsonantW -> vowelIndex v ["うぁ", "うぃ", "う", "うぇ", "うぉ"]
    ConsonantT -> 'て' : getHiraKana ConsonantLittleY v
    ConsonantD -> 'で' : getHiraKana ConsonantLittleY v

toHiraKanaChars (Just ConsonantY) c v =
  case c of
    ConsonantW -> vowelIndex v ["うゃ", "ゐ", "うゅ", "ゑ", "うょ"]
    ConsonantL -> getHiraKana ConsonantLittleY v
    ConsonantY -> getHiraKana ConsonantLittleY v
    ConsonantV -> 'ゔ' : getHiraKana ConsonantLittleY v
    ConsonantC -> 'ち' : getHiraKana ConsonantLittleY v
    _ -> getHiraKana c VowelI ++ getHiraKana ConsonantLittleY v


boin :: Parser Vowel
boin = oneOf "aiueo" >>= f
  where f 'a' = return VowelA
        f 'i' = return VowelI
        f 'u' = return VowelU
        f 'e' = return VowelE
        f 'o' = return VowelO

siin :: Parser Char
siin = noneOf "aiueon"

siinY :: Parser Consonant
siinY = oneOf "bfghjklmprvz" >>= f
  where f 'b' = return ConsonantB
        f 'f' = return ConsonantF
        f 'g' = return ConsonantG
        f 'h' = return ConsonantH
        f 'j' = return ConsonantJ
        f 'k' = return ConsonantK
        f 'l' = return ConsonantL
        f 'm' = return ConsonantM
        f 'p' = return ConsonantP
        f 'r' = return ConsonantR
        f 'v' = return ConsonantV
        f 'z' = return ConsonantZ

siinYH :: Parser Consonant
siinYH = oneOf "cdsw" >>= f
  where f 'c' = return ConsonantC
        f 'd' = return ConsonantD
        f 's' = return ConsonantS
        f 'w' = return ConsonantW

siinYHS :: Parser Consonant
siinYHS = char 't' >> return ConsonantT

optionY :: Parser Consonant
optionY = char 'y' >> return ConsonantY

optionYH :: Parser Consonant
optionYH = optionY <|> (char 'h' >> return ConsonantH)

optionYHS :: Parser Consonant
optionYHS = optionYH <|> (char 's' >> return ConsonantS)


romaji_char_wo_xtu :: Parser String
romaji_char_wo_xtu =
  (char 'y' >> (fmap $ toHiraKanaChars Nothing ConsonantY) boin)
  <|>
  (char 'q' >> toHiraKanaChars Nothing ConsonantQ <$> boin)
  <|>
  do
    c <- siinY
    o <- optionMaybe optionY
    toHiraKanaChars o c <$> boin
  <|>
  do
    c <- siinYH
    o <- optionMaybe optionYH
    toHiraKanaChars o c <$> boin
  <|>
  do
    c <- siinYHS
    o <- optionMaybe optionYHS
    toHiraKanaChars o c <$> boin
  <|> 
  do
    char 'x'
    (char 'n' >> return "ん")
      <|> 
      do
        o <- optionMaybe optionY 
        toHiraKanaChars o ConsonantX <$> boin
  <|>
  do
    char 'n'
    (char 'n' >> return "ん")
      <|> (eof >> return "ん")
      <|> (lookAhead (noneOf "aiueoyn") >> return "ん")
      <|>
      do
        o <- optionMaybe optionY
        toHiraKanaChars o ConsonantN <$> boin
  <|>
  toHiraKanaChars Nothing ConsonantNone <$> boin

romaji_char :: Parser String
romaji_char =
  try (do
          c <- siin
          (lookAhead . char) c
          ('っ' :) <$> romaji_char_wo_xtu
      )
  <|> romaji_char_wo_xtu


romaji_to_kana :: Parser String
romaji_to_kana = concat <$> many romaji_char
