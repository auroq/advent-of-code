module Main where

import System.IO


-- Check if the words are off by one letter in the same index.
-- Will still return False if the words are identical
offByOne :: String -> String -> Bool
offByOne = go False
  where go :: Bool -> String -> String -> Bool
        go off [] []  = off
        go off (x:xs) (y:ys)
          | x == y    = go off xs ys
          | otherwise = if off
                          then False
                          else go True xs ys
        go _ _ _ = error "go: words could not be compared"

permutate :: Eq a => [a] -> [(a, a)]
permutate wrds = [(x,y) | x <- wrds, y <- wrds, x /= y]

findBoxes = head . dropWhile (not . checkOffByOne) . permutate
  where checkOffByOne (x,y) = offByOne x y

getSame :: Eq a => [a] -> [a] -> [a]
getSame = go []
  where go :: Eq a => [a] -> [a] -> [a] -> [a]
        go acc [] [] = acc
        go acc (x:xs) (y:ys)
          | x == y    = go (x:acc) xs ys
          | otherwise = go    acc  xs ys

main :: IO ()
main = do
  contents <- getContents
  let boxes@(box1, box2) = findBoxes $ lines contents
  putStrLn $ show $ boxes
  putStrLn $ show $ reverse $ getSame box1 box2
