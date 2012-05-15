-- Romkan with Haskell.

module Romkan where

import Data.List
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
    "ぱぴぷぺぽ",
    "ぁぃぅぇぉ",
    "ゃぃゅぇょ"
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
  | ConsonantLittleY


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
    ConsonantL -> 15
    ConsonantX -> 15
    ConsonantLittleY -> 16


getHiraKana :: Consonant -> Vowel -> String
getHiraKana c v =
  case c of
    ConsonantF -> ["ふぁ", "ふぃ", "ふ", "ふぇ", "ふぉ"] !! getVowelOrder v
    ConsonantJ -> ["じゃ", "じ", "じゅ", "じぇ", "じょ"] !! getVowelOrder v
    ConsonantQ -> ["くぁ", "くぃ", "く", "くぇ", "くぉ"] !! getVowelOrder v
    ConsonantV -> ["ゔぁ", "ゔぃ", "ゔ", "ゔぇ", "ゔぉ"] !! getVowelOrder v
    _ -> [(listHiraKana !! getConsonantOrder c) !! getVowelOrder v]

toHiraKanaChars :: Maybe Consonant -> Consonant -> Vowel -> String
toHiraKanaChars Nothing c v = getHiraKana c v

toHiraKanaChars (Just ConsonantS) ConsonantT v =
  ["つぁ", "つぃ", "つ", "つぇ", "つぉ"] !! getVowelOrder v

toHiraKanaChars (Just ConsonantH) c v =
  case c of
    ConsonantC -> ["ちゃ", "ち", "ちゅ", "ちぇ", "ちょ"] !! getVowelOrder v
    ConsonantS -> ["しゃ", "し", "しゅ", "しぇ", "しょ"] !! getVowelOrder v
    ConsonantW -> ["うぁ", "うぃ", "う", "うぇ", "うぉ"] !! getVowelOrder v
    ConsonantT -> 'て' : getHiraKana ConsonantLittleY v
    ConsonantD -> 'で' : getHiraKana ConsonantLittleY v

toHiraKanaChars (Just ConsonantY) c v =
  case c of
    ConsonantW -> ["うゃ", "ゐ", "うゅ", "ゑ", "うょ"] !! getVowelOrder v
    ConsonantL -> getHiraKana ConsonantLittleY v
    ConsonantY -> getHiraKana ConsonantLittleY v
    ConsonantV -> 'ゔ' : getHiraKana ConsonantLittleY v
    ConsonantC -> 'ち' : getHiraKana ConsonantLittleY v
    _ -> getHiraKana c VowelI ++ getHiraKana ConsonantLittleY v


boin :: Parser Vowel
boin = oneOf "aiueo" >>= f
  where f c | c == 'a' = return VowelA
            | c == 'i' = return VowelI
            | c == 'u' = return VowelU
            | c == 'e' = return VowelE
            | c == 'o' = return VowelO

siin :: Parser Char
siin = noneOf "aiueon"

siinY :: Parser Consonant
siinY = oneOf "bfghjklmprvz" >>= f
  where f c | c == 'b' = return ConsonantB
            | c == 'f' = return ConsonantF
            | c == 'g' = return ConsonantG
            | c == 'h' = return ConsonantH
            | c == 'j' = return ConsonantJ
            | c == 'k' = return ConsonantK
            | c == 'l' = return ConsonantL
            | c == 'm' = return ConsonantM
            | c == 'p' = return ConsonantP
            | c == 'r' = return ConsonantR
            | c == 'v' = return ConsonantV
            | c == 'z' = return ConsonantZ

siinYH :: Parser Consonant
siinYH = oneOf "cdsw" >>= f
  where f c | c == 'c' = return ConsonantC
            | c == 'd' = return ConsonantD
            | c == 's' = return ConsonantS
            | c == 'w' = return ConsonantW

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
  do
    char 'y'
    v <- boin
    return $ toHiraKanaChars Nothing ConsonantY v
  <|>
  do
    char 'q'
    v <- boin
    return $ toHiraKanaChars Nothing ConsonantQ v
  <|>
  do
    c <- siinY
    o <- optionMaybe optionY
    v <- boin
    return $ toHiraKanaChars o c v
  <|>
  do
    c <- siinYH
    o <- optionMaybe optionYH
    v <- boin
    return $ toHiraKanaChars o c v
  <|>
  do
    c <- siinYHS
    o <- optionMaybe optionYHS
    v <- boin
    return $ toHiraKanaChars o c v
  <|> 
  do
    char 'x'
    (char 'n' >> return "ん")
      <|> 
      do
        o <- optionMaybe optionY 
        v <- boin
        return $ toHiraKanaChars o ConsonantX v
  <|>
  do
    char 'n'
    (char 'n' >> return "ん")
      <|> (eof >> return "ん")
      <|> (lookAhead (noneOf "aiueoyn") >> return "ん")
      <|>
      do
        o <- optionMaybe optionY
        v <- boin
        return $ toHiraKanaChars o ConsonantN v
  <|>
  do
    v <- boin
    return $ toHiraKanaChars Nothing ConsonantNone v

romaji_char :: Parser String
romaji_char =
  try (do
          c <- siin
          lookAhead $ char c
          x <- romaji_char_wo_xtu
          return $ 'っ' : x
      )
  <|> romaji_char_wo_xtu


romaji_to_kana :: Parser String
romaji_to_kana = do
  x <- many romaji_char
  return $ concat x
