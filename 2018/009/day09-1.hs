module Main where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map


play ::  Int -> Int -> Map Int Int
play numPlayers numMarbles = go (initPlayers) (cycle [1..numPlayers]) [0] [1..numMarbles]
  where go :: Map Int Int -> [Int] -> [Int] -> [Int] -> Map Int Int
        go scores _ _ [] = scores
        go scores (p:layers) [] (m:arbles)  = go scores layers [m]    arbles
        go scores (p:layers) [c] (m:arbles) = go scores layers [m, c] arbles
        go scores (p:layers) (c:i:rcle) (m:arbles)
          | m `mod` 23 == 0 = let (left, x, right) = takeR 7 (c:i:rcle)
                                  newScores = Map.adjust (+(m + x)) p scores
                              in go newScores layers (right ++ left) arbles
          | otherwise       = let circle = (m:rcle ++ [c] ++ [i])
                              in go scores layers circle arbles
        initPlayers = Map.fromList $ zip [1..numPlayers] $ cycle [0]


takeR n xs = let rev   = reverse xs
                 left  = reverse $ drop n rev
                 right = reverse $ take (n-1) rev
                 x     = head $ drop (n-1) rev
             in (left, x, right)


maxMap :: Map k Int -> Int
maxMap = Map.foldl (\acc v -> if acc < v then v else acc) (-1)

main :: IO ()
main = do
  contents <- getContents
  let prms = words contents
  let players = read $ input !! 0 :: Int
  let numMarbles = read $ input !! 1 :: Int
  let scores = play players numMarbles
  print scores
  let maxScore = maxMap scores
  print maxScore

