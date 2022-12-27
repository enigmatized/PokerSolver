
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}

module Main (main) where
import System.Random
import Test.QuickCheck
import CalculateWinner
import Data.Foldable
import Control.Applicative
import CalculateWinner
import DealCards
import Conduit
import Control.Monad
import System.Environment
import Debug.Trace
import Data.Array.IO
import Data.List

import Test.Hspec.Runner
import Data.Map.Strict (Map)
import qualified Data.Map                 as Map
import Data.List.Split 
import HelperFuncs



-- First 4 arguements are my cards
-- 5+6th arguements are gain vs loss gain 3 dollars to lose 2
-- 

--- Usually returns a list of two batch simulations [((Wins, Loses, ties), (Wins, Loses, Ties))]
-- Where the right hand side is river and left hand side is turn or flop 
--- This depends on how many cards provided
-- Note this is a for loop with numberOfSamples being.... 
monteCarloSimulation  :: Deck' -> Deck' -> [[Deck']] -> Deck' ->  Int -> Int -> IO [((Int, Int, Int), (Int, Int, Int))] 
monteCarloSimulation myCards houseCardDeck possibleEnemyStartingHands flop enemyDeckLength numberOfSamples = forM [1..numberOfSamples] $ \i -> do
      sliceer  <- generate  $  (choose (0, enemyDeckLength) :: Gen Int) -- This needs to get removed, this isn't random -- its choosing the same numbers in a 30 round
      --let sliceer = trace ("\nsclicer " ++ (show sliceer')) sliceer
      pure $ solveLazilyish  myCards houseCardDeck  ( take 30  $  drop sliceer possibleEnemyStartingHands) flop

-- Sums up all the triple tuples for a batch
-- This function can be thought as given 'n' combinations of enemy hands, give me the result from all hands.
solveLazilyish:: [(Char, Int)] -> [(Char, Int)] -> [[[(Char, Int)]]] -> [(Char, Int)] -> ((Int, Int, Int), (Int, Int, Int))
solveLazilyish  myCards deckOfCards allPossibleTwoPairsFromEnemies flop =   foldr'  (addTriplesMultiPair  . (gettOddsByTestingAgainstPossibleEnemyHands flop myCards deckOfCards)) ((0,0,0), (0,0,0)) allPossibleTwoPairsFromEnemies




-- TODO
--  Maybe splitting this off into threads or using deep seq

main :: IO ()
main = do

   args <- getArgs 
   putStrLn $ show args

   

   let myCards = [(head (args !! 0), (read (args !! 1) :: Int)), ((head (args !! 2)), (read (args !! 3) :: Int))]
   putStrLn $ ("My Cards    "  ++)  $ show myCards
   
   
   
   let d   =  removeCard' (makeDec' makeDec) (head (args !! 0)) (read (args !! 1) :: Int)
   let deckAfterMyCardsTakenOut  =  removeCard' d (head (args !! 2)) (read (args !! 3) :: Int)
   --putStrLn $ ("Deck after card removal"  ++)  $ show deckAfterMyCardsTakenOut

   let  wouldWin  = (read (args !! 4) :: Float)
   let wouldLose  = (read (args !! 5) :: Float)

   let numberOfPlayers  = (read (args !! 6) :: Int)
   let gamePlayPosition = (read (args !! 7) :: Int)
  


   let enemyDeckLength = (length worthyEnenmyStartingHands) - 30


   let xxxxxx = (\(a1, a2, a3) (a4, a5, a6) ->  (a1+ a4, a2 + a5, a3 + a6))

   -- let lsss = sort $ map (\(f,s) -> s ) [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )] 
   -- putStrLn $ ("Straight check hand " ++ ) $ show lsss
   -- putStrLn $ ("Straight check hand " ++ ) $ show $ countFiveInRow 1 (head lsss) 0 0  lsss
   -- n44 <- getLine
   -- putStrLn $ ("winning hand" ++ ) $ show $ findWinnerByBestHand [('H', 14 ), ('H', 13 ), ('H', 12 )] [('H', 10 ), ('H', 11 )] [('C', 10 ), ('C', 11 )]
   -- putStrLn $ ("map  looks like " ++ ) $ show $ (matchingFlushes $ sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])
   -- putStrLn $ ("filtermap looks like " ++ ) $ show $ Map.filter (\(a, b) -> a >= 5 ) (matchingFlushes $  sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])
   -- putStrLn $ ("Matching flushes" ++ ) $ show $ isHandValueFromFlush (matchingFlushes $ sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])   [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )]
   
   -- n2 <- getLine

   let communityCardsFlop = sort $ makeDeckFromCommmunity (drop 8 args) --Command line arguements must always have 8 arguements
   putStrLn $ (" communityCardsFlop  \n \n" ++) $ show  communityCardsFlop
   

   let newDeck = filter (\xxxxx -> not $  xxxxx `elem`  communityCardsFlop) deckAfterMyCardsTakenOut
   
   
   let myCardsPluscommunityCardsFlop = communityCardsFlop ++ myCards
   --putStrLn $ (" othter cards" ++) $ show myCardsPluscommunityCardsFlop
   
   clearnedUpWorthyEnenmyStartingHands' <- shuffle'' $ filter (\[a,b] -> not $ (a `elem` myCardsPluscommunityCardsFlop) || (b `elem` myCardsPluscommunityCardsFlop) ) worthyEnenmyStartingHands
   
   -- Note this should be modfied, to create more randomnessss. Probably in the batch part of the code
   let clearnedUpWorthyEnenmyStartingHands = chunksOf numberOfPlayers clearnedUpWorthyEnenmyStartingHands'
   --putStrLn $ ("  clearnedUpWorthyEnenmyStartingHands   " ++) $ show clearnedUpWorthyEnenmyStartingHands
   
   let enemyDeckLength = (length clearnedUpWorthyEnenmyStartingHands) -30

   let batchSize = if length communityCardsFlop == 0 then 8 else (if length communityCardsFlop == 3 then 30 else 40)
   
   newRes <-  monteCarloSimulation myCards newDeck clearnedUpWorthyEnenmyStartingHands communityCardsFlop enemyDeckLength batchSize
   putStrLn $ ("Results  " ++) $ show $ sort newRes
   let flop  = [aaa | (aaa,_ )<- newRes ]
   let river = [bbb | (_,  bbb )<- newRes]
   let winRatesCalculatedFlop  = showWinRates $ foldr  xxxxxx   (0, 0, 0)  flop
   let winRatesCalculatedRiver = showWinRates $ foldr  xxxxxx  (0, 0, 0)  river
   putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%)  " ++ ) $ show winRatesCalculatedFlop
   putStrLn $ (("Results from MONTE CARLO  Expected Value  " ++ (properEnding1 $ length communityCardsFlop ) ) ++)  $ show $ expectedValueIsh winRatesCalculatedFlop ( wouldWin ,  wouldLose )--((read wouldWin :: Int), (read wouldLose :: Int))
   putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) River " ++) $ show winRatesCalculatedRiver
   putStrLn $ (("Results from MONTE CARLO  Expected Value  " ++ (properEnding2 $ length communityCardsFlop )) ++) $ show $ expectedValueIsh winRatesCalculatedRiver (wouldWin , wouldLose)
