module Main where

import           System.IO
import           Data.List (sort)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map

type Step = (Char, String)

step :: String -> Step
step str = (go 7, [go 1])
  where go :: Int -> Char
        go i = head $ (flip (!!) i) $ words str


stepMap :: Ord k => Ord a => [(k, [a])] -> Map k [a]
stepMap steps = Map.map (sort) $ Map.fromListWith (++) steps


stepLetters :: Ord a => [(a, [a])] -> Set a
stepLetters steps = Set.fromList $ concat $ map (\(a, [b]) -> [a,b]) steps


hasDependency :: (Eq a, Foldable t) => t a -> [a] -> Bool
hasDependency installed =  any (\x -> not $ x `elem` installed)


perform :: Ord a => Map a [a] -> [a] -> [a]
perform depTree letters = reverse $ go [] depTree [] letters
  where go :: Ord a => [a] -> Map a [a] -> [a] -> [a] -> [a]
        go installed _       []      [] = installed
        go installed depTree skipped [] = go installed depTree [] (reverse skipped)  
        go installed depTree skipped (l:ls)
          | l `Map.member` depTree = if hasDependency installed $ depTree Map.! l
                                       then go installed depTree (l:skipped) ls
                                       else go (l:installed) (Map.delete l depTree) [] ((reverse skipped) ++ ls)
          | otherwise  = go (l:installed) depTree [] ((reverse skipped) ++ ls)


main :: IO ()
main = do
  contents <- getContents
  let steps = map step $ lines contents
  let depTree = stepMap steps
  let letters = Set.toList $ stepLetters steps
  let performance = perform depTree letters
  print steps
  print depTree
  print letters
  print performance
