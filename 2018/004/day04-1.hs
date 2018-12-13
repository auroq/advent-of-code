module Main where

import System.IO
import Data.Time
import Data.Map (Map)
import qualified Data.Map as Map

data Guard = Guard {id :: Int} deriving (Show, Eq, Ord)

data Line = Line UTCTime String deriving (Show, Eq)

instance Ord Line where
  (Line time1 _) `compare` (Line time2 _) = time1 `compare` time2

data Action = Begin Guard | Wakeup UTCTime | Sleep UTCTime deriving (Show)


skipTo :: Char -> String -> String
skipTo chr = tail . dropWhile (/= chr)


takeTo :: Char -> String -> String
takeTo chr = takeWhile (/= chr)


parseDate line = let date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %l:%M" $ takeDate line
                     noDate = skipDate line
                 in Line date noDate
                   where skipDate :: String -> String
                         skipDate = tail . skipTo ']'
                         takeDate :: String -> String
                         takeDate = takeTo ']' . tail


parseAction (Line time l) = 
  case words l of
    ["wakes", "up"]                       -> Wakeup time
    ["falls", "asleep"]                   -> Sleep time
    ["Guard", '#':num, "begins", "shift"] -> Begin $ Guard $ read num


mapActions :: [Action] -> Map Guard Int
mapActions = go Map.empty (0, Guard (-1), defaultTime)
  where go :: Map Guard Int -> (Int, Guard, UTCTime) -> [Action] -> Map Guard Int
        go acc last []     = acc
        go acc last (a:as) = let newLast@(val, guard, _) = performAction last a
                             in if Map.member guard acc
                                  then go (Map.adjust (+val) guard acc) newLast as
                                  else go (Map.insert guard val acc) newLast as


performAction :: (Int, Guard, UTCTime) -> Action -> (Int, Guard, UTCTime)
performAction (acc, lastGuard, lastTime) (Begin guard) = (0, guard, defaultTime)
performAction (acc, lastGuard, lastTime) (Wakeup time) = ((diffMinutes time lastTime), lastGuard, time)
performAction (acc, lastGuard, lastTime) (Sleep time)  = (0, lastGuard, time)


defaultTime :: UTCTime
defaultTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %l:%M" "1900-01-01 00:00"


diffMinutes time1 time2 = (minutes time1) - (minutes time2)
  where minutes time = let TimeOfDay _ minutes _ = timeToTimeOfDay $ utctDayTime time
                       in minutes


quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs


maxSleep :: Map Guard Int -> (Guard, Int)
maxSleep = Map.foldlWithKey (compareEntries) (Guard (-1), 0)
  where compareEntries :: (Guard, Int) -> Guard -> Int -> (Guard, Int)
        compareEntries acc guard sleep
          | (snd acc) < sleep = (guard, sleep)
          | otherwise         = acc


main :: IO ()
main = do
  contents <- getContents
  let sortedLines = quicksort $ map (parseDate) $ lines contents
  let actions = map parseAction sortedLines
  let actionMap =  mapActions actions
  let sleepyElf = maxSleep actionMap
  let part1 = ((Main.id . fst) sleepyElf) * (snd sleepyElf)
  putStrLn $ show $ actionMap
  putStrLn $ show $ sleepyElf
  putStrLn $ show $ part1

