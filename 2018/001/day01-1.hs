module Main where

import System.IO

getFrequency = foldl (+) 0

signedIntMaybe :: String -> Int
signedIntMaybe (s:n)
  | s == '+'  = read    n
  | otherwise = read (s:n)


convertLines :: String -> [Int]
convertLines input = 
  let allLines = lines input
      result   = map (signedIntMaybe) allLines
  in result


main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ getFrequency $ convertLines contents
