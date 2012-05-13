import Romkan

import Text.Parsec

main = do
  parseTest romaji_str "aiueo"
  parseTest romaji_str "kakikukeko"
  parseTest romaji_str "akasatanahamayarawa"
  parseTest romaji_str "naninuneno"
  parseTest romaji_str "konnnichiwa"
  parseTest romaji_str "toukyotokkyokyokakyoku"
  parseTest romaji_str "anshinsuru"
