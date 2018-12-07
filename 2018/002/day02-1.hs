module Main where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

checksum :: [String] -> Int
checksum wrds = (wordCountNRepeatedChars 2 wrds) * (wordCountNRepeatedChars 3 wrds)

wordCountNRepeatedChars :: Int -> [String] -> Int
wordCountNRepeatedChars n = length . filter (hasNRepeatedChars n)

hasNRepeatedChars :: Int -> String -> Bool
hasNRepeatedChars n = not . null . (Map.filter (== n)) . mapWord

mapWord :: String -> Map Char Int
mapWord = flip mapWord' Map.empty
  where mapWord' [] acc = acc
        mapWord' (l:ls) acc
          | l `Map.member` acc = mapWord' ls $ Map.adjust (+1) l acc
          | otherwise          = mapWord' ls $ Map.insert l 1 acc


main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ checksum $ lines contents
