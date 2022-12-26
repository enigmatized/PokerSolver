
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
--import HFlags
import Test.Hspec.Runner
import Data.Map.Strict (Map)
import qualified Data.Map                 as Map
import Data.List.Split 

-- Need to ask round pre flop, flop
-- Need to ask for number of players
-- 

removeCardsFromDeck :: Deck' -> Deck' -> Deck'
removeCardsFromDeck  ((a,b):xs) deck  =  removeCardsFromDeck xs $ removeCard' deck a b
removeCardsFromDeck  []    deck       = deck

makeDeckFromCommmunity :: [String] -> [(Char, Int)]
makeDeckFromCommmunity (y:(x:xs)) = [(head y, (read x :: Int))] ++ (makeDeckFromCommmunity xs)
makeDeckFromCommmunity []         =   []      

readString :: Int -> [String] ->  IO [String] 
readString 0 input = return input
readString n input = do
  z <- getLine
  readString (n-1) (z:input)

-- All two paris -->  Create all combinations of flops --> map calculate all winners  

--foldr addTriples (0,0,0) to a list of  winnerByPairs with myCards, house cards, enemyCards   

-- solveLazilyish:: [(Char, Int)] -> [(Char, Int)] -> [[(Char, Int)]] -> [[(Int, Int, Int)]] --(\enemyCards ->   (map (winnerByPairs' myCards enemyCards) )  .   )
-- solveLazilyish myCards deckOfCards allPossibleTwoPairsFromEnemies = map (\enemyCards ->    ( (foldr addTriples (0,0,0) ) $  map ( (winnerByPairs' myCards enemyCards)) $ (getAllFlopCombinations $ (removeCards'  deckOfCards enemyCards)))) allPossibleTwoPairsFromEnemies

-- solveLazilyish:: [(Char, Int)] -> [(Char, Int)] -> [[(Char, Int)]] -> (Int, Int, Int)
-- solveLazilyish myCards deckOfCards allPossibleTwoPairsFromEnemies =  (foldr addTriples (0,0,0) ) $ map (\enemyCards ->  (foldr addTriples (0,0,0) )  $  map ( (winnerByPairs' myCards enemyCards)) $ (getAllFlopCombinations $ (removeCards'  deckOfCards enemyCards))) allPossibleTwoPairsFromEnemies

solveLazilyish':: [(Char, Int)] -> [(Char, Int)] -> [[(Char, Int)]] -> (Int, Int, Int)
solveLazilyish' myCards deckOfCards allPossibleTwoPairsFromEnemies =   foldr'  (addTriples  . (makeFlopsAndGetOddsForFlops'  myCards deckOfCards)) (0,0,0) allPossibleTwoPairsFromEnemies


solveLazilyishFlopAndRiver:: [(Char, Int)] -> [(Char, Int)] -> [[(Char, Int)]] -> ((Int, Int, Int), (Int, Int, Int))
solveLazilyishFlopAndRiver myCards deckOfCards allPossibleTwoPairsFromEnemies =   foldr'  (addTriplesMultiPair  . (makeFlopsAndGetOddsForFlopsAndRiver  myCards deckOfCards)) ((0,0,0), (0,0,0)) allPossibleTwoPairsFromEnemies

solveLazilyishFlopProvided:: [(Char, Int)] -> [(Char, Int)] -> [[[(Char, Int)]]] -> [(Char, Int)] -> ((Int, Int, Int), (Int, Int, Int))
solveLazilyishFlopProvided myCards deckOfCards allPossibleTwoPairsFromEnemies flop =   foldr'  (addTriplesMultiPair  . (flopProvidedAndGetOddsForTurnAndRiver flop myCards deckOfCards)) ((0,0,0), (0,0,0)) allPossibleTwoPairsFromEnemies


solveLazilyishTurnRiver:: [(Char, Int)] -> [(Char, Int)] -> [[[(Char, Int)]]] -> [(Char, Int)] -> ((Int, Int, Int), (Int, Int, Int))
solveLazilyishTurnRiver myCards deckOfCards allPossibleTwoPairsFromEnemies flop =   foldr'  (addTriplesMultiPair  . (flopProvidedAndGetOddsForTurnAndRiver flop myCards deckOfCards)) ((0,0,0), (0,0,0)) allPossibleTwoPairsFromEnemies


expectedValueIsh :: (Float, Float, Float) -> (Float, Float) -> Float 
expectedValueIsh  (a, b, _) (c, d) = (a * (c)) + (b * ( d)) 

---OKay for this project what do I need to do next?
---Make the solver for winners:
---    look for flushes
---             Straights
--              triples
--              four of a kind



--  Maybe splitting this off into threads or using deep seq
--- Looking to optimize code quite  bit
--- 

--- Also doing this for flop calculations
--- ALso altering the deck so that the enemies hands are usually only play-able hands.....
--- It might be a good idea. Instead of doing all  hands is just playable hands. I think that limits the computations.
--- It might be good as well to instead of doing all calculations, do 500 hands instead of 12, because it takes a little too long.
--  

shuffle'' :: [a] -> IO [a]
shuffle'' xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


-- shuffle' :: (Eq a) => [a] -> IO [a]
-- shuffle' [] = return []
-- shuffle' ls = do
--     x <- pick ls
--     let y = remove x ls
--     xs <- shuffle y
--     return (x:xs)

n :: Int
n = 30

probWinning :: (Int, Int, Int) -> Float
probWinning (a1, a2, a3) =  (fromIntegral a1) / (fromIntegral (a1 + a2 + a3))

showWinRates :: (Int, Int, Int) -> (Float, Float, Float)
showWinRates (a1, a2, a3) = ((fromIntegral a1) / (fromIntegral (a1 + a2 + a3)),  (fromIntegral a2) / (fromIntegral (a1 + a2 + a3) ) ,  (fromIntegral a3) / (fromIntegral (a1 + a2 + a3)) )


monteCarloSamplingFlopOnly  :: Deck' -> Deck' -> [Deck'] ->  Int -> Int -> IO [(Int, Int, Int)] 
monteCarloSamplingFlopOnly myCards houseCardDeck possibleEnemyStartingHands enemyDeckLength numberOfSamples = forM [1..numberOfSamples] $ \i -> do
      sliceer  <- generate  $  (choose (0, enemyDeckLength) :: Gen Int)
      pure $ solveLazilyish' myCards houseCardDeck ( take 30  $  drop sliceer possibleEnemyStartingHands)

monteCarloSamplingFlopAndRiver  :: Deck' -> Deck' -> [Deck'] ->  Int -> Int -> IO [((Int, Int, Int), (Int, Int, Int))] 
monteCarloSamplingFlopAndRiver myCards houseCardDeck possibleEnemyStartingHands enemyDeckLength numberOfSamples = forM [1..numberOfSamples] $ \i -> do
      sliceer  <- generate  $  (choose (0, enemyDeckLength) :: Gen Int)
      pure $ solveLazilyishFlopAndRiver myCards houseCardDeck ( take 30  $  drop sliceer possibleEnemyStartingHands)

monteCarloProvidingTheFlop  :: Deck' -> Deck' -> [[Deck']] -> Deck' ->  Int -> Int -> IO [((Int, Int, Int), (Int, Int, Int))] 
monteCarloProvidingTheFlop myCards houseCardDeck possibleEnemyStartingHands flop enemyDeckLength numberOfSamples = forM [1..numberOfSamples] $ \i -> do
      sliceer  <- generate  $  (choose (0, enemyDeckLength) :: Gen Int) -- This needs to get removed, this isn't random -- its choosing the same numbers in a 30 round
      --let sliceer = trace ("\nsclicer " ++ (show sliceer')) sliceer
      pure $ solveLazilyishTurnRiver myCards houseCardDeck  ( take 30  $  drop sliceer possibleEnemyStartingHands) flop



main :: IO ()
main = do
   someFunc
   -- hspecWith (configAddFilter predicate defaultConfig) Spec.spec
   -- return ()
   -- https://www.fpcomplete.com/blog/2014/03/monte-carlo-haskell/
   -- let cnt = 10000000
   -- successes <- sourceRandomN cnt $$ lengthIfC (\(x, y) -> x*x + y*y < 1)
   -- print $ successes / cnt * 4


   
   -- eStartingCards <- shuffle worthyEnenmyStartingHands
   -- let enemyDeckLength = (length eStartingCards) -30
   -- newRes <- forM [1..n] $ \i -> do
   --    sliceer  <- generate  $  (choose (0, enemyDeckLength) :: Gen Int)
      
   --    solveLazilyish' myCards d2 ( take 30  $  drop sliceer worthyEnenmyStartingHands)




   



   -- putStrLn $ (show makeDec) ++"\n"
   -- putStrLn $ (show $  removeCard makeDec 'H' 3 ) ++ "\n"
   -- putStrLn $ (show $ map  (countFiveInRow 1 1)  [[1,2,3,4,5], [2,3,4,4,4,4,4] ]) 
 
   -- ggg  <- generate  $  (choose (0, 20) :: Gen Int)
   -- deck <- generate  $  (choose (1, 4) :: Gen Int)
   -- let suite = transFormToSuite deck
   

   --putStrLn    $ show  $  filter (\[(a ::Char, b :: Int), (c ::Char, d :: Int)] -> b == d ) $ getAllCombinationsWithTwoPair $ makeDec'  makeDec
   -- putStrLn $ ("\n" ++ ) $ show $ removeCard  makeDec  suite  ggg  
   -- putStrLn $ show (suite, ggg)
   -- deck2 <- getRandomCardFromDeck2 (makeDec, 54)
   -- putStrLn $ show $ (getAllCombinationsWithTwoPair . makeDec' ) makeDec
   -- putStrLn $ "\n\n\n\nshow"
   -- putStrLn $ show  worthyEnenmyStartingHands
   -- --strings <- readStrings
   -- s1 <- getLine
   -- n1 <- getLine
   -- s2 <- getLine
   -- n2 <- getLine

   -- wouldWin  <- getLine
   -- wouldLose <- getLine
   --let myCards = [((head s1), (read n1 :: Int)), ((head s2), (read n2 :: Int))]
   args <- getArgs 
   putStrLn $ show args

   

   let myCards = [(head (args !! 0), (read (args !! 1) :: Int)), ((head (args !! 2)), (read (args !! 3) :: Int))]
   putStrLn $ ("My Cards"  ++)  $ show myCards
   
   
   
   let d   =  removeCard' (makeDec' makeDec) (head (args !! 0)) (read (args !! 1) :: Int)
   let d2  =  removeCard' d (head (args !! 2)) (read (args !! 3) :: Int)

   putStrLn $ ("Deck after card removal"  ++)  $ show d2

   let  wouldWin  = (read (args !! 4) :: Float)
   let wouldLose  = (read (args !! 5) :: Float)

   let numberOfPlayers  = (read (args !! 6) :: Int)
   let gamePlayPosition = (read (args !! 7) :: Int)
  


   let enemyDeckLength = (length worthyEnenmyStartingHands) -30


   let xxxxxx = (\(a1, a2, a3) (a4, a5, a6) ->  (a1+ a4, a2 + a5, a3 + a6))

   let lsss = sort $ map (\(f,s) -> s ) [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )] 
   putStrLn $ ("Straight check hand " ++ ) $ show lsss
   putStrLn $ ("Straight check hand " ++ ) $ show $ countFiveInRow 1 (head lsss) 0 0  lsss
   n44 <- getLine
   putStrLn $ ("winning hand" ++ ) $ show $ findWinnerByBestHand [('H', 14 ), ('H', 13 ), ('H', 12 )] [('H', 10 ), ('H', 11 )] [('C', 10 ), ('C', 11 )]
   putStrLn $ ("map  looks like " ++ ) $ show $ (matchingFlushes $ sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])
   putStrLn $ ("filtermap looks like " ++ ) $ show $ Map.filter (\(a, b) -> a >= 5 ) (matchingFlushes $  sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])
   putStrLn $ ("Matching flushes" ++ ) $ show $ isHandValueFromFlush (matchingFlushes $ sort [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )])   [('H', 14 ), ('H', 13 ), ('H', 12 ), ('H', 10 ), ('H', 11 )]
   
   n2 <- getLine
   case  gamePlayPosition of 
      1 -> do

               let clearnedUpWorthyEnenmyStartingHands = filter (\[a,b] -> not $ (a `elem` myCards) || (b `elem` myCards) ) worthyEnenmyStartingHands
               let enemyDeckLength = (length clearnedUpWorthyEnenmyStartingHands) -30
               newRes <-  monteCarloSamplingFlopAndRiver myCards d2 worthyEnenmyStartingHands enemyDeckLength 10
               let flop = [aaa | (aaa,_ )<- newRes ]
               let river = [bbb | (_,  bbb )<- newRes]
               let winRatesCalculatedFlop =  showWinRates $ foldr  xxxxxx   (0, 0, 0)  flop
               let winRatesCalculatedRiver =  showWinRates $ foldr  xxxxxx  (0, 0, 0)  river
               putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) flop" ++) $ show winRatesCalculatedFlop
               putStrLn $ ("Results from MONTE CARLO  Expected Value flop" ++) $ show $ expectedValueIsh winRatesCalculatedFlop ( wouldWin ,  wouldLose )--((read wouldWin :: Int), (read wouldLose :: Int))
               putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) flop" ++) $ show winRatesCalculatedRiver
               putStrLn $ ("Results from MONTE CARLO  Expected Value flop" ++) $ show $ expectedValueIsh winRatesCalculatedRiver (wouldWin , wouldLose)
            
      _ -> do

               
               let communityCardsFlop = sort $ makeDeckFromCommmunity (drop 8 args)
               putStrLn $ (" communityCardsFlop  \n \n" ++) $ show  communityCardsFlop
               let newDeck = filter (\xxxxx -> not $  xxxxx `elem`  communityCardsFlop) d2

               putStrLn $ (" Deck \n \n" ++) $ show   newDeck


               putStrLn $ ("\n\n Deck2 \n \n" ++) $ show   d2
               let myCardsPluscommunityCardsFlop = communityCardsFlop ++ myCards
               putStrLn $ (" othter cards" ++) $ show myCardsPluscommunityCardsFlop
               --let clearnedUpWorthyEnenmyStartingHands = filter (\[a,b] -> not $ (a `elem` myCardsPluscommunityCardsFlop) || (b `elem` myCardsPluscommunityCardsFlop) ) worthyEnenmyStartingHands
               clearnedUpWorthyEnenmyStartingHands' <- shuffle'' $ filter (\[a,b] -> not $ (a `elem` myCardsPluscommunityCardsFlop) || (b `elem` myCardsPluscommunityCardsFlop) ) worthyEnenmyStartingHands
               let clearnedUpWorthyEnenmyStartingHands = chunksOf numberOfPlayers clearnedUpWorthyEnenmyStartingHands'
               putStrLn $ ("  clearnedUpWorthyEnenmyStartingHands   " ++) $ show clearnedUpWorthyEnenmyStartingHands
               let enemyDeckLength = (length clearnedUpWorthyEnenmyStartingHands) -30
               --Do you need enemyDeckLength?
               newRes <-  monteCarloProvidingTheFlop myCards newDeck clearnedUpWorthyEnenmyStartingHands communityCardsFlop enemyDeckLength 20
               putStrLn $ ("Results  " ++) $ show $ sort newRes
               let flop = [aaa | (aaa,_ )<- newRes ]
               let river = [bbb | (_,  bbb )<- newRes]
               let winRatesCalculatedFlop =  showWinRates $ foldr  xxxxxx   (0, 0, 0)  flop
               let winRatesCalculatedRiver =  showWinRates $ foldr  xxxxxx  (0, 0, 0)  river
               putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) Turn " ++) $ show winRatesCalculatedFlop
               putStrLn $ ("Results from MONTE CARLO  Expected Value Turn " ++) $ show $ expectedValueIsh winRatesCalculatedFlop ( wouldWin ,  wouldLose )--((read wouldWin :: Int), (read wouldLose :: Int))
               putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) River " ++) $ show winRatesCalculatedRiver
               putStrLn $ ("Results from MONTE CARLO  Expected Value River " ++) $ show $ expectedValueIsh winRatesCalculatedRiver (wouldWin , wouldLose)
      
      -- _ -> do
      --          let communityCardsFlop = makeDeckFromCommmunity (drop 8 args)
      --          let newDeck = removeCardsFromDeck d2 communityCardsFlop

      --          let myCardsPluscommunityCardsFlop = communityCardsFlop ++ myCards

      --          let clearnedUpWorthyEnenmyStartingHands = filter (\[a,b] -> not $ (a `elem` myCardsPluscommunityCardsFlop) || (b `elem` myCardsPluscommunityCardsFlop) ) worthyEnenmyStartingHands
      --          let enemyDeckLength = (length clearnedUpWorthyEnenmyStartingHands) -30
      --          --Do you need enemyDeckLength?
      --          newRes <-  monteCarloProvidingTheFlop myCards newDeck clearnedUpWorthyEnenmyStartingHands communityCardsFlop enemyDeckLength 20
               
      --          let flop = [aaa | (aaa,_ )<- newRes ]
      --          let river = [bbb | (_,  bbb )<- newRes]
      --          let winRatesCalculatedFlop =  showWinRates $ foldr  xxxxxx   (0, 0, 0)  flop
      --          let winRatesCalculatedRiver =  showWinRates $ foldr  xxxxxx  (0, 0, 0)  river
      --          putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) flop" ++) $ show winRatesCalculatedFlop
      --          putStrLn $ ("Results from MONTE CARLO  Expected Value flop" ++) $ show $ expectedValueIsh winRatesCalculatedFlop ( wouldWin ,  wouldLose )--((read wouldWin :: Int), (read wouldLose :: Int))
      --          putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) flop" ++) $ show winRatesCalculatedRiver
      --          putStrLn $ ("Results from MONTE CARLO  Expected Value flop" ++) $ show $ expectedValueIsh winRatesCalculatedRiver (wouldWin , wouldLose)


      -- 3 ->
      -- 4 ->
   -- --putStrLn $ "\n---   " ++ (show d2 ) ++"\n"

   -- Now calculate all 3 cards with combinatin of two cards
   -- About 21187600 combinations
--    let allTwoPa = (getAllCombinationsWithTwoPair d2)
-- --    let enemyCard = (head allTwoPa)

-- --    let allflops = getAllFlopCombinations (removeCards'  d2 enemyCard)
-- --    let hmmmmm = map (\x  ->  winnerByPairs  x myCards enemyCard) allflops
-- --    putStrLn $ show $ allflops
-- --    putStrLn $ show $ hmmmmm
-- --    putStrLn $ show $ foldr addTriples (0,0,0)  hmmmmm
   

   --THIS IS FOR ALL COMBINATIONS OF TWO PAIRS
   --putStrLn $   "REsults " ++ (show $ solveLazilyish myCards d2 (getAllCombinationsWithTwoPair d2))  --ATTEMPT # ? Not sure, but
   -- let results = solveLazilyish' myCards d2 (getAllCombinationsWithTwoPair d2)
   -- putStrLn $ "Results" ++ (show $  results)

   --THIS is for just worthy enemy hands

   --eStartingCards <- shuffle worthyEnenmyStartingHands
   --worthyEnenmyStartingHands' <- shuffle' worthyEnenmyStartingHands

   -- let hmmm = (\(a1, a2, a3) -> (a1 /n , a2 / n , a3 / n))
   --putStrLn $ ("Results from MONTE CARLO  " ++)$  show $ (\(a1, a2, a3)-> (fromIntegral a1) / (fromIntegral (a1 + a2 + a3) ))   $ foldr  xxxxxx  (0, 0, 0)  newRes
   
   -- newRes <- monteCarloSampling myCards d2 worthyEnenmyStartingHands enemyDeckLength 20
   -- let winRatesCalculated =  showWinRates $ foldr  xxxxxx  (0, 0, 0)  newRes
   -- putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%)" ++) $ show winRatesCalculated
   -- putStrLn $ ("Results from MONTE CARLO  Expected Value" ++) $ show $ expectedValueIsh winRatesCalculated ((read wouldWin :: Int), (read wouldLose :: Int))


   -- let results = solveLazilyish' myCards d2 (worthyEnenmyStartingHands)
   -- putStrLn $ "Results" ++ (show $  results)

------------------------------------------------------
--FLOP AND RIVER calculation
   





------------------------------------------------------
-- Old flop Solver-- First one I made
-----------------------------------------------------
   -- let results = solveLazilyish' myCards d2 (worthyEnenmyStartingHands)
   -- putStrLn $ "Results" ++ (show $  results)
   -- putStrLn $ (" Probs winning " ++) $ show $ showWinRates results





--    putStrLn $ show $ 
--    putStrLn $ show $ )
   --let flopsLegnth = round $ (fromIntegral $ length allTwoPa) / 10.0
   --let allCombinationFlopsRelatedToCards =  map (\enenyCards -> getAllFlopCombinations (removeCards'  d2 enenyCards)) (take flopsLegnth allTwoPa)
   --let enemyCardsPairedWIthAllFlopCombos = zip (take flopsLegnth allTwoPa)  allCombinationFlopsRelatedToCards
   --let allFlopsByTwoCards = map (\(en, awllFlops)  ->  foldr addTriples (0,0,0) $ map  (\jjj -> winnerByPairs  jjj myCards en) awllFlops) enemyCardsPairedWIthAllFlopCombos
   --putStrLn $ show $ foldr addTriples (0,0,0) $ allFlopsByTwoCards

   --let results = foldr addTriples (0,0,0) $ allFlopsByTwoCards

    --All two paris
    --

    --OOOO I could do it this way
    --Create a two pair 

   --let results = foldr addTriples (0,0,0) $  map (\(en, awllFlops)  ->  foldr addTriples (0,0,0) $ map  (\jjj -> winnerByPairs  jjj myCards en) awllFlops) $  zip (allTwoPa)    $ map (getAllFlopCombinations . (removeCards'  d2)) allTwoPa




--    let results =  calcPairs myCards enemyCard  d2 
--    --let results = allCalc myCards (getAllCombinationsWithTwoPair d2) d2
--    putStrLn $ show $ getPairs [('a', 2), ('a', 3), ('a', 4), ('b', 2)] [ ('a', 4), ('b', 5)]
--    putStrLn $ show $ winnerByPairs   [('a', 2), ('a', 3), ('a', 4), ('b', 2)] [ ('a', 4), ('b', 5)]  [ ('a', 11), ('b', 9)] 
     

--    let allTwoPairs =  getAllCombinationsWithTwoPair d2
--    let opponentCards = (head allTwoPairs)

--    let d3 = removeCards' d2 opponentCards
   

   
--    let flops   = getAllNCombinations d3 3
-- --    (putStrLn . show . length) flops
-- --    let flopsLegnth = round $ (fromIntegral $ length flops) / 100.0
-- --    let smallerFlop = take flopsLegnth flops
   
--    let results = foldr addTriples (0,0,0)  $ map (\x  ->  winnerByPairs x myCards opponentCards) smallerFlop   
   --putStrLn $ show results
   
    --  foldr (addTriples) (0,0,0) $ 
    --So we need a counnt of who wins, 
    --So out of amount
    --Ties as well
    --putStrLn "Probs"
--    putStrLn $   "REsults " ++ (show results)

   --putStrLn $  "Probs of winning" ++ (show $  results)
   --putStrLn $  "Probs of Losing" ++ (show $ (\(a1, a2, a3)-> (fromIntegral a2) / (fromIntegral (a1 + a2 + a3) )) results)
   --putStrLn $  "Probs of tie-ing" ++ (show $ (\(a1, a2, a3)-> (fromIntegral a3) / (fromIntegral (a1 + a2 + a3)) ) results)
   



    --Then go head to head for parirs


   


   --allCombs xs = [1..] >>= \n -> mapM (const xs) [1..n] -- https://stackoverflow.com/questions/21775378/get-all-possible-combinations-of-k-elements-from-a-list 

   
   --putStrLn $ show $ getRandomCardFromDeck2 (getRandomCardFromDeck2 (makeDec, 54), 53)
   

   -- Given two cards, odds of winning vs random two cards
   -- 

-- Okay the next thing to test is two pair
   
