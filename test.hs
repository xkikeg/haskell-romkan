import Romkan
import Data.List
import Text.Parsec

prod :: [a] -> [b] -> [(a, b)]
prod xs ys = concatMap (f ys) xs
  where f ys' x = map (\y -> (x, y)) ys'

hirakanaTest :: [Consonant] -> [Vowel] -> String
hirakanaTest cs vs = concatMap f $ prod cs vs
  where f (c, v) = toHiraKanaChars Nothing c v

allVowel = [VowelA, VowelI, VowelU, VowelE, VowelO]

main = do
  putStrLn $ hirakanaTest [ConsonantNone, ConsonantP, ConsonantW] allVowel
  parseTest romaji_str "aiueo"
  parseTest romaji_str "kakikukeko"
  parseTest romaji_str "akasatanahamayarawa"
  parseTest romaji_str "naninuneno"
  parseTest romaji_str "koxnnichiwa"
  parseTest romaji_str "toukyotokkyokyokakyoku"
  parseTest romaji_str "annan"
  parseTest romaji_str "antabaka"
  parseTest romaji_str "sapporo"
