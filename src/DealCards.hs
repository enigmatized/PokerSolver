{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts #-}

module DealCards where

import Data.List
--import Data.Map    (Map)
import Test.QuickCheck
import Data.Foldable
import Data.Maybe
import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map                 as Map


type Deck = [(Char, [Int])]
type Deck' = [(Char, Int)]

makeDec :: Deck
makeDec =[ (y,  [x | x <- [2 .. 14] ]) | y <- ['S', 'H', 'C', 'D']]

makeDec' :: Deck -> Deck'
makeDec' deck = concat $ map (\(y, ls) -> map (y,) ls) deck


worthyEnenmyStartingHands :: [[(Char, Int)]]
worthyEnenmyStartingHands  = catMaybes  $ map filterForOnlyStrongEnemyHand   $ (getAllCombinationsWithTwoPair . makeDec' ) makeDec

worthyEnenmyStartingHands' :: [Maybe [(Char, Int)]]
worthyEnenmyStartingHands'  = map (\x -> ( (matchTwoPair x) <|> (sameSuite x) <|> (oneLargeCards x)) )  $ (getAllCombinationsWithTwoPair . makeDec' ) makeDec


filterForOnlyStrongEnemyHand :: [(Char, Int)]  -> Maybe [(Char, Int)]
filterForOnlyStrongEnemyHand [(a, b), (c, d)]
    | b == d      = Just [(a, b), (c, d)]
    | b ==  d + 1 = Just [(a, b), (c, d)]           
    | a == c      = Just [(a, b), (c, d)]
    | otherwise = Nothing

sameSuite :: [(Char, Int)] -> Maybe [(Char, Int)]
sameSuite [(a, b), (c, d)] 
    | b == d      = Just [(a, b), (c, d)]
    | otherwise = Nothing

matchTwoPair :: [(Char, Int)] -> Maybe [(Char, Int)]
matchTwoPair [(a, b), (c, d)] 
    | b ==  d + 1 = Just [(a, b), (c, d)]
    | otherwise = Nothing

oneLargeCards :: [(Char, Int)] -> Maybe [(Char, Int)]
oneLargeCards [(a, b), (c, d)] 
    | b == d      = Just [(a, b), (c, d)]
    | otherwise = Nothing


transFormToSuite :: Int -> Char
transFormToSuite x  = case x of 1 -> 'H'; 2 -> 'S'; 3 -> 'C'; 4 -> 'D';

getRandomCardFromDeck :: (Deck, Int)  -> IO (Deck', Int)
getRandomCardFromDeck (deck, cardCount)  = do
    ggg  <- generate  $  (choose (1, 14) :: Gen Int)
    suit <- generate  $  (choose (1, 4) :: Gen Int)
    let suite = transFormToSuite suit

    pure $ ( makeDec' $ removeCard  deck suite ggg , cardCount - 1 )

getRandomCardFromDeck2 :: (Deck, Int)  -> IO (Deck', Int)
getRandomCardFromDeck2 (deck, cardCount)  = do
    ggg  <- generate  $  (choose (1, cardCount) :: Gen Int)
    pure $ ( removeCardByInt (makeDec' deck) ggg , cardCount - 1 )

deal2Cards :: (Deck, Int)  -> IO (Deck', Int)
deal2Cards (deck, cardCount)  = do
    ggg  <- generate  $  (choose (1, cardCount) :: Gen Int)
    pure $ (  removeCardByInt (makeDec' deck) ggg , cardCount - 1 )



removeCards' :: Deck' -> [(Char, Int)] -> Deck'
removeCards' deck toRemoveCards = filter (not . (`elem` toRemoveCards)) deck  


removeCard' :: Deck' -> Char -> Int -> Deck'
removeCard' deck suite cardNum = filter ((suite, cardNum) /= ) deck  


removeCard :: Deck -> Char -> Int -> Deck
removeCard deck suite cardNum = [ if y == suite then (y, (filter (cardNum /=) x)) else (y,x)  | (y, x)  <- deck ]


getAllCombinationsWithTwoPair :: Deck' -> [[(Char, Int)]]
getAllCombinationsWithTwoPair deck = [x| x <- mapM (const deck ) [1..2], (snd $  head x) <= ( snd $ head  (tail x)) ]


getAllFlopCombinations :: Deck' -> [[(Char, Int)]]
getAllFlopCombinations deck  = [x| x <- mapM (const deck ) [1..3], (snd $  head x) <= ( snd $ head  (tail x)) ]


getAllNCombinations :: Deck' -> Int -> [[(Char, Int)]]
getAllNCombinations deck   n  = [x| x <- mapM (const deck ) [1..n], (snd $  head x) <= ( snd $ head  (tail x)) ]

--removeCardByInt :: [(Char,[Int] )] -> Int -> [(Char, [Int])]
--removeCardByInt deck removeIndex =   (\items -> take removeIndex items ++ drop (1 + removeIndex) deck) deck

removeCardByInt :: Deck' -> Int -> Deck'
removeCardByInt deck removeIndex =   (\items -> take removeIndex items ++ drop (1 + removeIndex) deck) deck 



