import qualified Data.Map as Map
import Data.Maybe

digits = [0..9]

onesWords = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
tensWords = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

onesMap = Map.fromList $ zip digits onesWords
tensMap = Map.fromList $ zip digits tensWords

onesToWord :: Int -> String
onesToWord n = fromJust $ Map.lookup n onesMap

tensToWord :: Int -> String
tensToWord n = fromJust $ Map.lookup n tensMap
