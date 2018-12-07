module Main where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)

class Grid a where
  grid :: a -> [Position]

data Rectangle = Rectangle {pos :: Position, width :: Int, height :: Int} deriving (Show, Eq)

data Claim = Claim {id :: Int, rectangle :: Rectangle} deriving (Show, Eq)

instance Grid Rectangle where
  grid (Rectangle (x, y) width height) = [(px, py) | px <- [x..(x+width-1)], py <- [y..(y+height-1)]]


parseLine :: String -> Claim
parseLine line = let id     = read $ takeTo ' ' $ tail line              :: Int
                     (x,y)  = read $ "(" ++ (takeTo ':' $ tail $ skipTo '@' line) ++ ")" :: (Int, Int)
                     width  = read $ takeTo 'x' $ skipTo ':' line        :: Int
                     height = read $ skipTo 'x' line                     :: Int
                 in Claim id $ Rectangle (x,y) width height
                   where skipTo :: Char -> String -> String
                         skipTo chr = tail . dropWhile (/= chr)
                         takeTo :: Char -> String -> String
                         takeTo chr = takeWhile (/= chr)

claims :: [String] -> [Claim]
claims = map (parseLine)


grids :: [String] -> [[Position]]
grids = map (grid . rectangle) . claims


allSquares :: [String] -> [Position]
allSquares = foldl (++) [] . grids


numOverlap lines = let gs       = allSquares lines
                       overlaps = Map.filter (> 1) $ mapOverlap gs
                   in length overlaps

mapOverlap :: Ord a => [a] -> Map a Int
mapOverlap = go Map.empty
  where go :: Ord k => Map k Int -> [k] -> Map k Int
        go acc [] = acc
        go acc (x:xs)
          | x `Map.member` acc = go (Map.adjust (+1) x acc) xs
          | otherwise          = go (Map.insert x 1 acc) xs

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ numOverlap $ lines contents
