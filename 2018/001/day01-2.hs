module Main where

import System.IO

repeatingInput :: [a] -> [a]
repeatingInput = concat . repeat

getDuplicate :: [Int] -> Int
getDuplicate fs = go (0, [0]) $ repeatingInput fs
  where go _  [] = error ""
        go (acc, prev) (x:xs) =
          let newAcc = acc + x in
          if newAcc `elem` prev
            then newAcc
            else go (newAcc, newAcc:prev) xs

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
  putStrLn $ show $ getDuplicate $ convertLines contents
