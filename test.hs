import Romkan

import Text.Parsec

main = do
  parseTest romaji_str "aiueo"
  parseTest romaji_str "kakikukeko"
  parseTest romaji_str "akasatana"
  parseTest romaji_str "konnnichiwa"
