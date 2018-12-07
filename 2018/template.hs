module Main where

import System.IO


main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ 
