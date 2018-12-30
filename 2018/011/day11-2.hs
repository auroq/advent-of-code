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


getnxn n grid (Cell (x,y) _) = let pos = getnxnPos n (x,y)
                               in map ((Map.!) grid) pos
  where getnxnPos n (x,y) = [(x', y') | x' <- [x..(x+n-1)], y' <- [y..(y+n-1)], x' <= 300, y' <= 300]


allnxnGrids :: Int -> Grid -> Map Position Int
allnxnGrids n grid = Map.map (sumCells . (getnxn n grid)) grid
  where sumCells :: [Cell] -> Int
        sumCells = (sum . map power)


allGrids :: Grid -> Map Int (Map Position Int)
allGrids grid = Map.fromList $ map (\n -> (n, allnxnGrids n grid)) [1..300]


mapMax :: Map Position Int -> (Position, Int)
mapMax = Map.foldlWithKey (\(acck, accv) k c -> if c > accv then (k,c) else (acck, accv)) ((-1,-1), minBound :: Int)

allMaxes :: Map Int (Map Position Int) -> Map Int (Position, Int)
allMaxes = Map.map (mapMax)


maxOfMaxes :: Map Int (Position, Int) -> ((Int, Position), Int)
maxOfMaxes = Map.foldlWithKey compareVals ((-1,(-1,-1)), minBound :: Int)
  where compareVals :: ((Int, Position), Int) -> Int -> (Position, Int) -> ((Int, Position), Int)
        compareVals (acck, accv) n (p, v) = if v > accv
                                              then ((n,p), v)
                                              else (acck, accv)


main :: IO ()
main = do
  contents <- getContents
  let sn = read contents :: Int
  let grid = genGrid sn
  let allSums = allGrids grid
  let allMxs = allMaxes allSums
  let max = maxOfMaxes allMxs
  print max


