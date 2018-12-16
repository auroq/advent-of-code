module Main where

import System.IO

data Node = Node {header :: (Int, Int), children :: [Node], metaData :: [Int]} deriving (Show, Eq)


tree :: [Int] -> [Node]
tree [] = []
tree (c:m:xs)
  | c > 0     = let (newxs, meta) = splitR m xs
                in [Node (c, m) (tree newxs) (meta)]
  | otherwise = let (meta, newxs) = splitL m xs
                in (Node (c, m) [] (meta)):(tree newxs)


-- Take n items from the left
splitL :: Int -> [Int] -> ([Int], [Int])
splitL n xs = let left  = take n xs
                  right = drop n xs
              in (left, right)

-- Take n items from the right
splitR n xs = let rev = reverse xs
                  left = reverse $ drop n rev
                  right = reverse $ take n rev
              in (left, right)


sumMeta :: [Node] -> Int
sumMeta [] = 0
sumMeta (n:ns) = (sum $ metaData n) + sumMeta(ns) + (sumMeta $ children n)

allMeta :: [Node] -> [Int]
allMeta [] = [] 
allMeta (n:ns) = (metaData n) ++ allMeta(ns) ++ (allMeta $ children n)


main :: IO ()
main = do
  contents <- getContents
  let input = map read $ words contents :: [Int]
  let inputTree = tree input
  let total = sumMeta inputTree
  let allMetas = allMeta inputTree
  print inputTree
  print allMetas
  print total
  print "done"
