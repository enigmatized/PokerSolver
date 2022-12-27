{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts #-}

{-# Language PatternSynonyms #-}


module CalculateWinner where

import Data.List
--import Data.Map    (Map)
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

import Debug.Trace



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




--- So this is a issue.
-- Way too much duplicate calculations
-- How do I deal with this?
-- Just turn flop to 5 card and make the moteo carlo simulation smaller
-- 
-- makeFlopsAndGetOddsForFlopsAndRiver :: Deck' -> Deck' -> Deck' -> ( (Int, Int, Int), (Int, Int, Int))
-- makeFlopsAndGetOddsForFlopsAndRiver myCards  deck enemyCards   = foldl' addTriplesMultiPair ((0,0,0), (0,0,0))  [ | x <- mapM (const deck ) [1..5], (snd $  head x) < ( snd $ head  (tail x)) ]


-- (findWinnerByBestHand myCards enemyCards (take 3 x), findWinnerByBestHand myCards enemyCards x) 

-- Get a hand
--
--
--


-- elemFix ::
-- elemFix ::

-- removeCards'NotFancyHaskell :: Deck' -> [(Char, Int)] -> Deck'
-- removeCards'NotFancyHaskell deck toRemoveCards = filter (not . (`elem` toRemoveCards))  deck  




--removeCardByInts :: Deck' -> [Int] -> ( Deck' , Deck' )
--removeCardByInts deck removeIndex =  ( (map (\index -> take index deck ++ drop (1 + removeIndex) deck)  removeIndex), (map ( deck !!) removeIndex ) )

removeCardByInts :: (Deck', Deck') -> [Int] -> ( Deck' , Deck' )
removeCardByInts (deck, res) (x:xs) = removeCardByInts (take x deck ++ drop (1 + x) deck, res ++ [ (deck !! x) ] )  xs  
removeCardByInts deck []  =   deck 


--( (map (\index -> take index

--findPairs :: [(Char, [Int]] -> [ ]  
--findPairs
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


-- -- This function should only be given sorted from least to greatest cards
-- countFiveInRowForAllCards ::  [Int] -> Maybe  Int
-- countFiveInRowForAllCards ls
--     | length ls == 5 =  countFiveInRow 0 (head ls) 




--- TODO Write this function that solves StraightFlush, Flush and Straight in o(n)
-- countFiveInRow'' :: Int -> Int -> Int -> Int ->  [(Char, Int)] ->  -- (IsStraight, last StraightValue, isStraightFlush, flushLast)
-- countFiveInRow'' curStraightCount straightCount highestStraight f last  ls 
--     | ls == [] && count >= 5      = Just last
--     | ls == [] && count < 5       = Nothing
--     | count == 5                  = Just last
--     | count < 5 && length ls == 0 = Nothing
--     | (last + 1 == head ls)       = countFiveInRow (count+1) (head ls) (tail ls)
--     | last == head ls             = countFiveInRow count last $ tail ls  
--     | (last + 1 < head ls) && count < 5       = Nothing
--     | (last + 1 < head ls) && count >= 5       =  Just last
--     | otherwise = Nothing



--





-- This is a really complicated calculation that I think could be calculated in one function with pattern matching
--I can easily make this function check for triples and quads.

-- getPairs :: [Int] -> [Int] -> [(Int, Int)]
-- getPairs communityCards [a, b] 
--     |  a == b    =   [( a, (2 +) $ length $ intersect communityCards [a]  )] ++ [communityCards \\ [a, b]]
--     |    
--     where 
--         communityCardsWithNotCountingDealtCards = [communityCards \\ [a, b]]


--Hmmm I need to create a new type
--Something that can be compared to each otherwis

mySortTuplesByLastWithGreatestAsHead :: Ord b => [(a, b)] -> [(a, b)]
mySortTuplesByLastWithGreatestAsHead = sortBy (flip compare `on` snd)

mySortTuplesByFstWithGreatestAsHead :: Ord a => [(a, b)] -> [(a, b)]
mySortTuplesByFstWithGreatestAsHead = sortBy (flip compare `on` fst)




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




    -- nub $ (intersect  x  y) ++ (x \\ (nub x)) ++ (y \\ (nub y))
    -- where
    --      !x  = map (\(f,s) -> s )  xx
    --      !y  = map (\(f,s) -> s )  yy



-- (wins, losses, ties, outOf)


---OH Wow this logic is fucked up.
-- I am not calculating if the community card case
-- I am starting to see, to calculate somethng to the  River
-- It has to be a monte carlo simulation to be able to do in live games.
-- If I get this working tonight.
-- I could probably make the monte Carlo Simulation part work rather fast  
-- I am starting to think that 
-- winnerByPairs :: Deck' -> Deck' -> Deck' -> (Int, Int, Int)  -- (win, loss, tie)
-- winnerByPairs houseCards myCards enemyCards   
--     | length myPairs > length theirPairs            = (1, 0, 0)
--     | length myPairs < length theirPairs            = (0, 1, 0)
--     | length myPairs == 0 && length theirPairs == 0 = (0,0,0) --Check highest card
--     | length myPairs == length theirPairs           = compareValues (last myPairs) (last theirPairs)

--     where
--         tform = map (\(f,s) -> s )
--         houseCards'  = sort $ tform  houseCards
--         myCards'     = sort $ tform  myCards
--         enemyCards'  = sort $ tform  enemyCards
--         myPairs    = getPairs houseCards' myCards'
--         theirPairs = getPairs houseCards' enemyCards'
--         compareValues :: Int -> Int -> (Int, Int, Int)
--         compareValues x y = if x > y then (1, 0, 0) else (if x <  y then (0, 1, 0) else (0,0,1)) 
 

-- highestCardCheck :: Deck' -> Deck'   -> (Int, Int, Int)
-- highestCardCheck myCards enemyCards  = 


--THIS IS A GOOD FUNCTION
-- winnerByPairs' :: Deck' -> Deck' -> Deck' -> (Int, Int, Int)  -- (win, loss, tie)
-- winnerByPairs' !myCards !enemyCards !houseCards   
--     | length myPairs > length theirPairs            = (1, 0, 0)
--     | length myPairs < length theirPairs            = (0, 1, 0)
--     | length myPairs == 0 && length theirPairs == 0 = (0,0,0) --Check highest card
--     | length myPairs == length theirPairs           = compareValues (last myPairs) (last theirPairs)
--     where
--         myPairs    = sort $  getPairs houseCards myCards
--         theirPairs = sort $  getPairs houseCards enemyCards
--         compareValues :: Int -> Int -> (Int, Int, Int)
--         compareValues x y = if x > y then (1, 0, 0) else (if x <  y then (0, 1, 0) else (0,0,1)) 


-- Note this only gets winner, it does not tell me what winner is
-- Or how I coould win the next hand
-- These are just flops
-- Also this is not ideal.
-- It's almost like there should be a function seperate for each
-- So if no pair
-- 

-- Before this is called
-- The community cards should be matched 
-- and added to the pairs or triples maps
-- Also the hands dealt

-- winnersByMatchingOrHighCard :: Deck' -> Deck' -> Deck' -> Map String [Int] -> Map String [Int] -> Map String [Int] -> (String, String) -> (Int, Int, Int)  -- (win, loss, tie)
-- winnersByMatchingOrHighCard [(a, b), (c, d)] [(e, f), (g, h)] []  highCards piars triples winner  = case winner of ('Me', _) -> (1,0,0);  ('Enemy', _) -> (1,0,0); _ -> (0,0,1);
-- winners [(_, b), (_, d)] [(_, f), (_, h)] (x:xs)  highCards piars triples winner = (0,0,0) 
--        | (b == x) || (d == x)  &&  (f == x) || (h /= x)     = (0,0,0) 
--     -- |
--     -- | 
--        where
--             if Map.memeber cardNum piars





--countFiveInRow :: Int -> (Int,Int) -> [(Int, Int)] -> (Bool, Int)
--countFiveInRow 5 
--countFiveInRow count compare rest = if (fst compare ) == fst head rest then 

allCalc :: [(Char, Int)] -> [[(Char, Int)]] -> [(Char, Int)] -> (Int, Int, Int)
allCalc  myCards allPossibleTwoPairs deck =    foldr addTriples (0,0,0) $ map (calcPairs myCards deck)  allPossibleTwoPairs

calcPairs :: [(Char, Int)] ->  [(Char, Int)] -> [(Char, Int)] -> (Int, Int, Int)
calcPairs  myCards  deck enemyCards =  foldr addTriples (0,0,0)  $ map (\x  ->  findWinnerByBestHand  x myCards enemyCards)  $ getAllFlopCombinations (removeCards'  deck enemyCards)


