module Main where

import           System.IO
import           Data.Map (Map)
import qualified Data.Map as Map


type Point = (Int, Int)


parseXY  :: String -> Point
parseXY xy = read $ "(" ++ xy ++ ")"


points :: [String] -> [Point]
points = map parseXY


gridMax :: [Point] -> (Int, Int)
gridMax pts = ((maximum $ map fst pts) + 1, (maximum $ map snd pts) + 1)

gridMin :: [Point] -> (Int, Int)
gridMin pts = ((minimum $ map fst pts) - 1, (minimum $ map snd pts) - 1)

grid :: (Int, Int) -> (Int, Int) -> [Point]
grid (minX, minY) (maxX, maxY) = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]

fromPts :: [Point] -> [Point]
fromPts pts = grid (gridMin pts) (gridMax pts)



distance :: Point -> Point -> Int
distance (p1X, p1Y) (p2X, p2Y) = (abs (p2Y - p1Y)) + (abs (p2X - p1X))


--mapDistances :: [Point] -> [Point] -> Map Point (Map Point Int)
--mapDistances = pGo Map.empty
--  where pGo :: Map Point (Map Point Int) -> [Point] -> [Point] -> Map Point (Map Point Int)
--        pGo pAcc [] _         = pAcc
--        pGo pAcc (pt:pts) grd = let newPAcc = Map.insert pt (gGo Map.empty pt grd) pAcc
--                                in pGo newPAcc pts grd
--
--        gGo :: Map Point Int -> Point -> [Point] -> Map Point Int
--        gGo gAcc _ []      = gAcc
--        gGo gAcc pt (g:gs) = let newGAcc = Map.insert g (distance pt g) gAcc
--                             in gGo newGAcc pt gs


mapDistances :: [Point] -> [Point] -> (Map Int Int, Map Point (Map Point Int))
mapDistances = pGo Map.empty Map.empty
  where pGo :: Map Point (Map Point Int) -> Map Int Int ->[Point] -> [Point] -> (Map Int Int, Map Point (Map Point Int))
        pGo pAcc distances [] _         = (distances, pAcc)
        pGo pAcc distances (pt:pts) grd = let (newDistances, newGGo) = gGo Map.empty distances pt grd
                                              newPAcc                = Map.insert pt newGGo pAcc
                                          in pGo newPAcc  newDistances pts grd
        
        gGo :: Map Point Int -> Map Int Int -> Point -> [Point] -> (Map Int Int, Map Point Int)
        gGo gAcc distances _ []      = (distances, gAcc)
        gGo gAcc distances pt (g:gs) = let d       = distance pt g
                                           newGAcc = Map.insert g d gAcc
                                           newDs   = if Map.member d distances
                                                       then Map.adjust (+1) d distances 
                                                       else Map.insert d 1 distances
                                       in gGo newGAcc newDs pt gs

--filterDuplicates :: Map Point (Map Point Int) -> Map Point (Map Point Int)
--filterDuplicates = Map.filter (not $ (== Map.empty))
--  where go :: [Int] -> Map Point Int -> Map Point Int
--        go distances grd = Map.filter (not $ flip elem distances) grd


--mapDistances coor (pt:pts) = Map.insert pt 
--mapDistances :: [Point] -> [Point] -> [((Point, Point), Int)]
--mapDistances pts grd = distance <$> pts <*> grd


rmElem _ [] = []
rmElem x (y:ys)
  | x == y    = rmElem x ys
  | otherwise = y : rmElem x ys


main :: IO ()
main = do
  contents <- getContents
  let pts = points $ lines contents
  let grd = fromPts pts
  --let distances = mapDistances pts grd
  putStrLn $ show $ gridMin pts
  putStrLn $ show $ gridMax pts
  --putStrLn $ show $ length distances
