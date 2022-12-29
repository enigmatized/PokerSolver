import System.Random
import Test.QuickCheck
import CalculateWinner
import Data.Foldable
import Control.Applicative

import DealCards
import Conduit
import Control.Monad
import System.Environment
import Debug.Trace
import Data.Array.IO
import Data.List
import CardTypes
import CalculateWinner

import Test.Hspec
import Data.Maybe


main :: IO ()
main = hspec spec



checkStraightCheckerTooSmall :: [[Int]] -> Bool  
checkStraightCheckerTooSmall ls = ([] ==) $ catMaybes $ map  (\y -> if length y > 0 then countFiveInRow 1 (head y) 0 0  y else Nothing)  $ map (\x -> if  length x > 4 then sort $ drop 4 x else sort x) ls


checkFlush :: [[Int]] -> Bool  
checkFlush ls = ([] ==) $ catMaybes $ map  (\y -> if length y > 0 then countFiveInRow 1 (head y) 0 0  y else Nothing)  $ map (\x -> if  length x > 4 then sort $ drop 4 x else sort x) ls


spec :: Spec
spec = describe "Need to write more here" $ do
    it "countFiveInARow testing a straights" $ do
        let lsss = sort $ map (\(f,s) -> s ) [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )] 
        (Just 14) == countFiveInRow 1 (head lsss) 0 0  lsss

    -- it "countFiveInARow testing a straights" $ do
    --     let lsss = sort $ map (\(f,s) -> s ) [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )] 
    --     (Just 14) == countFiveInRow 1 (head lsss) 0 0  lsss
    it "If cards provided to Straight Checker are too small, should always provide nothing" $
            quickCheck checkStraightCheckerTooSmall



   -- let lsss = sort $ map (\(f,s) -> s ) [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )] 
   -- putStrLn $ ("Straight check hand " ++ ) $ show lsss
   -- putStrLn $ ("Straight check hand " ++ ) $ show $ countFiveInRow 1 (head lsss) 0 0  lsss
   -- n44 <- getLine
   -- putStrLn $ ("winning hand" ++ ) $ show $ findWinnerByBestHand [('H', 14 ), ('H', 13 ), ('H', 12 )] [('H', 10 ), ('H', 11 )] [('C', 10 ), ('C', 11 )]
   -- putStrLn $ ("map  looks like " ++ ) $ show $ (matchingFlushes $ sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])
   -- putStrLn $ ("filtermap looks like " ++ ) $ show $ Map.filter (\(a, b) -> a >= 5 ) (matchingFlushes $  sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])
   -- putStrLn $ ("Matching flushes" ++ ) $ show $ isHandValueFromFlush (matchingFlushes $ sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])   [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )]
   
   -- n2 <- getLine
