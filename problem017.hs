import Data.Text (Text, unpack)
import qualified Text.Numeral.Language.ENG as EN
import Text.Numeral.Grammar (defaultInflection)
import Data.Maybe (fromJust)

numberLetterCounts :: Int
numberLetterCounts = sum $ map (length . stripNum . unpack . fromJust) l
    where limit = 1000
          l = [ EN.gb_cardinal defaultInflection x :: Maybe Text | x <- [1..limit]]
          stripNum xs = [ x | x <- xs, not (x `elem` " -") ]

-- numberLetterCounts == 18451

-- ENG.gb_cardinal defaultInflection 123 :: Maybe Text
-- Just "one hundred twenty-three" but should show 
-- "one hundred and twenty-three"

