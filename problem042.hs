import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as M

alphaMap = M.fromList $ zip ['A'..'Z'] [1..]

getLetterValue :: Char -> Int
getLetterValue c = fromJust $ M.lookup c alphaMap

getWordValue :: String -> Int
getWordValue = foldl (\acc v -> acc + getLetterValue v) 0 

getWordList :: String -> [String]
getWordList = splitOn "," . init

getTriangleNumbers :: Int -> [Int]
getTriangleNumbers x = takeWhile (<=x) $ map (\n -> (n * (n + 1)) `div` 2) [1..]

main :: IO ()
main = do
  content <- readFile "problem042.txt"
  let wv = map getWordValue $ getWordList content
  let tn = getTriangleNumbers $ maximum wv 
  putStrLn $ show $ length [x | x <- wv, x `elem` tn]

