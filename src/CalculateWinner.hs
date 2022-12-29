{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts #-}

{-# Language PatternSynonyms #-}


module CalculateWinner where

import Data.List
import Test.QuickCheck
import Data.Foldable
import Data.Maybe
import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map                 as Map
import CardTypes
import Data.Function (on)
import DealCards
import HelperFuncs

#import Debug.Trace



gettOddsByTestingAgainstPossibleEnemyHands :: Deck' -> Deck' -> Deck' -> [Deck'] -> ( (Int, Int, Int), (Int, Int, Int))
gettOddsByTestingAgainstPossibleEnemyHands    flop    myCards   deck    enemyCards    = do
    case length flop of
        0 ->  (foldl' addTriples (0,0,0) noCardsProvidedRandomFlops, foldl' addTriples (0,0,0) noCardsProvidedRandomRivers)
        3 -> do  --trace (show [(x:flop) | x <- deck ] ) 
            --let tripleList = ([  checkForWinnersOrLoser (map (findWinnerByBestHand (x:flop) myCards) enemyCards)  | x <- deck ])
            let fiveList   = [checkForWinnersOrLoser $ map (findWinnerByBestHand (x ++ flop) myCards) enemyCards  | x <- mapM (const deck ) [1..2], (snd $  head x) < ( snd $ head  (tail x)) ]
            (foldl' addTriples (0,0,0) tripleList, foldl' addTriples (0,0,0) fiveList)
        4 -> do
            (foldl' addTriples (0,0,0) tripleList, (1,1,1))
        _ -> (foldl' addTriples (0,0,0) [checkForWinnersOrLoser $ map  (findWinnerByBestHand flop myCards) enemyCards  ], (0,0,0))
    
    where
        checkForWinnersOrLoser :: [(Int, Int, Int)] -> (Int, Int, Int)
        checkForWinnersOrLoser ls = if (0,1,0) `elem` ls then (0,1,0) else (if (0,0,1) `elem` ls then (0,0,1) else (1,0,0))
        tripleList = [ checkForWinnersOrLoser $ map (findWinnerByBestHand (x:flop) myCards)  enemyCards  | x <- deck ]
        noCardsProvidedRandomFlops = [  checkForWinnersOrLoser $ map (findWinnerByBestHand x myCards) enemyCards | x <- mapM (const deck ) [1..3], (snd $  head x) < ( snd $ head  (tail x)) ]
        noCardsProvidedRandomRivers = [ checkForWinnersOrLoser $ map (findWinnerByBestHand x myCards) enemyCards  | x <- mapM (const deck ) [1..5], (snd $  head x) < ( snd $ head  (tail x)) ]



removeCardByInts :: (Deck', Deck') -> [Int] -> ( Deck' , Deck' )
removeCardByInts (deck, res) (x:xs) = removeCardByInts (take x deck ++ drop (1 + x) deck, res ++ [ (deck !! x) ] )  xs  
removeCardByInts deck []  =   deck 

--TODO !!!!!!
--Note to caller, for this to work the list of nums should be sorted
--Need to add a check for ace 2 3 4 5 straight
countFiveInRow :: Int -> Int -> Int -> Int -> [Int] -> Maybe  Int
countFiveInRow count last largestCount largestStraight  ls 
    | ls == [] &&  largestCount >= 5      = Just largestStraight
    | ls == [] &&  largestCount  < 5      = Nothing
    -- | count == 5                  = Just last
    -- | count < 5 && length ls == 0 = Nothing
    | (last + 1 == head ls)       = countFiveInRow (count+1) (head ls) (max (count +1) largestCount) (if (count+1) > largestCount then head ls else largestStraight)  (tail ls)
    | last == head ls             = countFiveInRow count last largestCount largestStraight $ tail ls  
    | (last + 1 < head ls)       = countFiveInRow 1 (head ls) largestCount largestStraight $ tail ls  
    | (last + 1 > head ls)       = Nothing
    -- | otherwise = Nothing

mySortTuplesByLastWithGreatestAsHead :: Ord b => [(a, b)] -> [(a, b)]
mySortTuplesByLastWithGreatestAsHead = sortBy (flip compare `on` snd)

mySortTuplesByFstWithGreatestAsHead :: Ord a => [(a, b)] -> [(a, b)]
mySortTuplesByFstWithGreatestAsHead = sortBy (flip compare `on` fst)



-- This sometimes errors?
-- I have no clue why?
-- Says un-exastive pattern
-- Seems like that would be the case but that should never actually happen. 
-- Maybe the deck is not being cleaned properly so there is more than a 4 of a kind happening?!
-- I have to test if there are two four of a kinds, then larges is what's picked first.
matchingCardsToHand'' :: [(Int, Int)] -> HandValue
matchingCardsToHand''  ( (a,b):xs)  
    | b   == 4 = FourOfAKind a
    | b == 3 = if xs == [] then (Triple a) else (case head xs of (c, 2) -> Fullhouse (b, c); _ -> (Triple a)  )  
    | b == 2 = if xs == [] then (Pair a) else (case head xs of (c, 2) -> TwoPair (b, c); _ -> (Pair a)  )  
    | b == 1 = HighCard $ fst $ head $ mySortTuplesByFstWithGreatestAsHead ( (a,b):xs)



-- This takes in just the number value of a cards as a list
-- Return a Map of count of each
-- Basically Defaultdict in python
matchingCards :: [Int] -> Map Int Int
matchingCards (x:xs)   = Map.insertWith (+)  x 1 $  matchingCards xs
matchingCards []       = Map.empty

matchingFlushes :: Deck' -> Map Char (Int,Int) -- (Int,Int) == (count, largestValue)
matchingFlushes ((a,b):xs)   = Map.insertWith (\(c, d) (e, f) -> (e+c , max d f ))  a (1, b) $  matchingFlushes xs
matchingFlushes []           = Map.empty


--This assume if if sorted? No it fulters
isHandValueFromFlush ::  Map Char (Int,Int) -> Deck' -> Maybe HandValue 
isHandValueFromFlush dic deck' = case Map.toList (Map.filter (\(a, b) -> a >= 5 ) dic) of 
        [(k, (a, b))] -> do
            let justListInts = sort $ map (\(f,s) -> s ) $ filter (\(c, d) -> c == k ) deck'
            case countFiveInRow 1 (head justListInts) 0 0  justListInts of 
                    Nothing ->  Just $ Flush b
                    Just _  ->  Just $ StraightFlush b
        [] -> Nothing 



--TODO check if community beats everyone -- then tie -- This is only good if community cards is 5 or more cards
--Maybe remove the duplicate sort and reverse.
-- Probably wise to reverse everything
-- Or even wiser, sort it by largest to smallest in one go.....
findWinnerByBestHand ::  Deck' -> Deck' -> Deck' -> (Int, Int, Int)
findWinnerByBestHand houseCards myCards enemyCards
    | myStrongestHand == theirStrongestHand =  --Need to check if the community hand is the same 
        --trace ("\n " ++ (show myStrongestHand) ++ "  from  " ++ (show myCards) ++"\n" ++  (show theirStrongestHand) ++"  from  " ++ (show enemyCards) ++ " community cards "  ++ (show houseCards) ++  "\n\n"  )  
        (case myStrongestHand of
            Pair     a       -> resFromHighestCard ( removePairFromHand myCards'' a)  ( removePairFromHand enemyCards'' a)
            TwoPair  (a, b)  -> resFromHighestCard (removeTwoPairFromHand myCards'' a b) (removeTwoPairFromHand enemyCards'' a b)
            Triple   a  -> resFromHighestCard (removeTripleFromHand myCards'' a) (removeTripleFromHand enemyCards'' a)
            HighCard a -> resFromHighestCard (removeHighCard myCards'' a)  (removeHighCard enemyCards'' a)
            -- HighCard a ->
            _ -> (0, 0, 1))-- trace ("\n " ++ (show houseCards) ++"\n" ++  (show myCards) ++ "   " ++ (show enemyCards))  (0, 0, 1)
    | myStrongestHand > theirStrongestHand  = (1, 0, 0)
    | myStrongestHand < theirStrongestHand  = (0, 1, 0)

    where 
        tform = map (\(f,s) -> s )
        houseCards'   = tform  houseCards
        -- houseCards' = houseCards
        myCards'      = tform   myCards
        enemyCards'   = tform   enemyCards
        -- Should I be reverse sorting this? So that they are [3,2,1] not [1,2,3]
        myCards''     = sort $  houseCards' ++ myCards'
        enemyCards''  = sort $  houseCards' ++ enemyCards'
        myHandByPairs = matchingCardsToHand'' $ (mySortTuplesByLastWithGreatestAsHead . Map.toList . matchingCards) $  myCards''
        theirHandPairs     = matchingCardsToHand'' $ (mySortTuplesByLastWithGreatestAsHead . Map.toList . matchingCards) $ enemyCards'' 
        --community   = matchingCardsToHand'' $ (mySortTuplesByLastWithGreatestAsHead . Map.toList . matchingCards) $ enemyCards'' 
        myStrongHands = (isHandValueFromFlush  (matchingFlushes $ houseCards ++ myCards) (houseCards ++ myCards)) <|> (case countFiveInRow 1 (head myCards'') 0  0  myCards'' of
            Nothing -> Nothing
            Just i  -> Just $ Straight i)
        myStrongestHand = max (fromMaybe myHandByPairs myStrongHands) myHandByPairs

        theirStrongHands = (isHandValueFromFlush  (matchingFlushes $ houseCards ++ enemyCards) (houseCards ++ enemyCards)) <|> (case countFiveInRow 1 (head enemyCards'') 0  0  enemyCards'' of --
            Nothing -> Nothing
            Just i  -> Just $ Straight i)
        theirStrongestHand = max (fromMaybe theirHandPairs theirStrongHands) theirHandPairs

        communityPairs = matchingCardsToHand'' $ (mySortTuplesByLastWithGreatestAsHead . Map.toList . matchingCards) $ houseCards'
        communityStrongHands = (isHandValueFromFlush  (matchingFlushes $ houseCards) (houseCards ++ myCards)) <|> (case countFiveInRow 1 (head myCards'') 0 0  myCards'' of  -- This is wrong
            Nothing -> Nothing
            Just i  -> Just $ Straight i)
        --High Card removal
        removeHighCard  cards c1             =  take 4 $ reverse (cards \\ [c1] )
        --Pairs removal
        removePairFromHand cards c1         =  take 3 $ reverse (cards \\ [c1,c1] )
        removeTwoPairFromHand cards c1 c2   = head $ reverse (cards \\ [c1,c1,c2,c2] )
        resFromHighestCard miHighest thierHighest = if   miHighest > thierHighest then (1,0,0) else (if miHighest < thierHighest then (0,1,0) else (0,0,1))
        --Triple Removal
        removeTripleFromHand cards c1 =  take 2 $ reverse (cards \\ [c1,c1, c1] )
        -- myStraights =
        -- myHand     = if myHandByPairs > 




allCalc :: [(Char, Int)] -> [[(Char, Int)]] -> [(Char, Int)] -> (Int, Int, Int)
allCalc  myCards allPossibleTwoPairs deck =    foldr addTriples (0,0,0) $ map (calcPairs myCards deck)  allPossibleTwoPairs

calcPairs :: [(Char, Int)] ->  [(Char, Int)] -> [(Char, Int)] -> (Int, Int, Int)
calcPairs  myCards  deck enemyCards =  foldr addTriples (0,0,0)  $ map (\x  ->  findWinnerByBestHand  x myCards enemyCards)  $ getAllFlopCombinations (removeCards'  deck enemyCards)


