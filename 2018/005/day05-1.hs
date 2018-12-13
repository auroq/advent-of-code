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

main :: IO ()
main = do
  contents <- getContents
  let reacted = reverse $ react $ stripEnd $ contents
  putStrLn reacted
  putStrLn $ show $ length reacted
