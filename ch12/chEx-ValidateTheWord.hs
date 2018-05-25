module ValidateTheWord where

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = if numConsonants < numVowels then Nothing else Just $ Word' s
  where numVowels = length $ filter (`elem` vowels) s
        numConsonants = (length s) - numVowels
