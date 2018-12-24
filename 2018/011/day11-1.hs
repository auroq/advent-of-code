module Main where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)

data Cell = Cell {position :: Position, power :: Int} deriving (Show)

type Grid = Map Position Cell

instance Eq Cell where
  (Cell _ pa) == (Cell _ pb) = pa == pb

instance Ord Cell where
  compare (Cell _ pa) (Cell _ pb) = compare pa pb


genGrid :: Int -> Grid
genGrid sn = go sn (1,1) Map.empty
  where go :: Int -> Position -> Grid -> Grid
        go sn (x,y) grid
          | y > 300   = grid
          | x > 300   = go sn (1, y + 1) grid
          | otherwise = let cell = powerLevel sn (x,y)
                        in go sn (x + 1, y) $ Map.insert (x,y) cell grid


powerLevel :: Int -> Position -> Cell
powerLevel sn (x, y) = let rkid = x + 10
                           pwr1 = rkid * y
                           pwr2 = pwr1 + sn
                           pwr3 = pwr2 * rkid
                           pwr4 = read $ ((reverse $ show pwr3) !! 2):[] :: Int
                           pwrf = pwr4 - 5
                       in Cell (x,y) pwrf


get3x3 grid (Cell (x,y) _) = let pos = get3x3Pos (x,y)
                             in map ((Map.!) grid) pos
  where get3x3Pos (x,y) = [(x', y') | x' <- [x..x+2], y' <- [y..y+2], x' <= 300, y' <= 300]


allGrids :: Grid -> Map Position Int
allGrids grid = Map.map (sumCells . get3x3 grid) grid


sumCells :: [Cell] -> Int
sumCells = (sum . map power)


mapMax :: Map Position Int -> (Position, Int)
mapMax = Map.foldlWithKey (\(acck, accv) k c -> if c > accv then (k,c) else (acck, accv)) ((-1,-1), minBound :: Int)


main :: IO ()
main = do
  contents <- getContents
  let sn = read contents :: Int
  let grid = genGrid sn
  let allSums = allGrids grid
  let max = mapMax allSums
  putStrLn $ show $ max


