module Main where

import System.IO
import Data.Char (toUpper,toLower,isSpace)


toggleCase :: Char -> Char
toggleCase c
  | c >= 'a' && c <= 'z' = toUpper c
  | otherwise            = toLower c


react :: String -> String
react str = go [] str
  where go acc []   = acc
        go acc [c1] = c1:acc
        go acc (c1:c2:cs)
          | c1 == toggleCase c2 = go []       ((reverse acc) ++ cs)
          | otherwise           = go (c1:acc) (c2:cs)


stripEnd :: String -> String
stripEnd = reverse . dropWhile isSpace . reverse


removedUnits :: String -> [String]
removedUnits lst = map (flip removeUnit lst) ['a'..'z']


removeUnit :: Char -> String -> String
removeUnit unit = filter (\u -> u /= unit && (toggleCase u) /= unit)


shortest :: Eq a => [[a]] -> [a]
shortest = foldl (\acc x -> if acc == [] || length x < length acc then x else acc) []


main :: IO ()
main = do
  contents <- getContents
  let input = stripEnd contents
  let removed = removedUnits input
  let reacted = map react removed
  let short = minimum $ map length reacted
  putStrLn $ show short
