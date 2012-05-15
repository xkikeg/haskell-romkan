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

myparseTest p s = case parse p "test" s of
  Left err -> print err
  Right xs -> putStrLn xs

main = do
  putStrLn $ hirakanaTest [ConsonantNone, ConsonantP, ConsonantW] allVowel
  myparseTest romaji_to_kana "aiueo"
  myparseTest romaji_to_kana "kakikukeko"
  myparseTest romaji_to_kana "akasatanahamayarawa"
  myparseTest romaji_to_kana "naninuneno"
  myparseTest romaji_to_kana "koxnnichiwa"
  myparseTest romaji_to_kana "toukyoutokkyokyokakyoku"
  myparseTest romaji_to_kana "annan"
  myparseTest romaji_to_kana "antabaka"
  myparseTest romaji_to_kana "sapporo"
  myparseTest romaji_to_kana "damm"
