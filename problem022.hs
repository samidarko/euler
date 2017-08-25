import Text.Regex.Posix
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)


namesScores :: IO ()
namesScores = do
  content <- readFile "problem022.txt"
  putStrLn $ show $ process content

process :: String -> Int
process xs = foldl (\acc v -> (sum $ map e (fst v)) * snd v + acc) 0 (zip l [1..])
  where
    l = sort $ getAllTextMatches $ xs =~ "[A-Z]+" :: [String]
    m = M.fromList $ zip ['A'..'Z'] [1..]
    e = (fromJust . flip  M.lookup m)

