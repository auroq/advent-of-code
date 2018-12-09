module Main where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)

class Grid a where
  grid :: a -> [Position]
  overlap :: a -> a -> [Position]
  overlaps :: a -> [a] -> [(a, [Position])]
  hasOverlap :: a -> a -> Bool
  hasOverlaps :: a  -> [a] -> Bool


overlaps' :: Grid a => Eq a => [(a, [Position])] -> a -> [a] -> [(a, [Position])]
overlaps' acc _ [] = acc
overlaps' acc a (b:bs)
  | a == b    = overlaps' acc a bs
  | otherwise = let ol = overlap a b
                in if not $ null ol
                     then overlaps' ((b, ol):acc) a bs
                     else overlaps' acc a bs


data Rectangle = Rectangle {pos :: Position, width :: Int, height :: Int} deriving (Show, Eq)

data Claim = Claim {id :: Int, rectangle :: Rectangle} deriving (Show, Eq)

instance Grid Rectangle where
  grid (Rectangle (x, y) width height) = [(px, py) | px <- [x..(x+width-1)], py <- [y..(y+height-1)]]
  overlap a b = filter (\x -> x `elem` (grid b)) $ grid a
  overlaps = overlaps' []
  hasOverlap a b = not $ null $ overlap a b
  hasOverlaps a bs = not $ null $ overlaps a bs

instance Grid Claim where
  grid (Claim _ a) = grid a
  overlap a b = overlap (rectangle a) (rectangle b)
  overlaps = overlaps' []
  hasOverlap a b = hasOverlap (rectangle a) (rectangle b)
  hasOverlaps a bs = not $ null $ overlaps a bs


type ClaimList = [Claim]


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


-- Recursively look for a claim with no overlaps and return it
noOverlaps :: [Claim] -> Claim
noOverlaps clms = go clms clms
  where go :: [Claim] -> [Claim] -> Claim
        go _ [] = error "noOverlaps: Could not locate claim without overlaps"
        go clms (c:cs) =  if not $ hasOverlaps c clms
                          then c
                          else go clms cs


main :: IO ()
main = do
  contents <- getContents
  let clms = claims $ lines contents
  putStrLn $ show $ noOverlaps clms
