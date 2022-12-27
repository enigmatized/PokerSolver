module CodeGraveYardWhereFuncsGoToDie where


--    -- 
--    case  length communityCardsFlop of -- I do not think this needs to be added. It can be infered by cards given
--       1 -> do
--                -- let clearnedUpWorthyEnenmyStartingHands = filter (\[a,b] -> not $ (a `elem` myCards) || (b `elem` myCards) ) worthyEnenmyStartingHands
--                -- let enemyDeckLength = (length clearnedUpWorthyEnenmyStartingHands) -30
--                -- newRes <-  monteCarloSamplingFlopAndRiver myCards d2 worthyEnenmyStartingHands enemyDeckLength 10
--                -- let flop = [aaa | (aaa,_ )<- newRes ]
--                -- let river = [bbb | (_,  bbb )<- newRes]
--                -- let winRatesCalculatedFlop =  showWinRates $ foldr  xxxxxx   (0, 0, 0)  flop
--                -- let winRatesCalculatedRiver =  showWinRates $ foldr  xxxxxx  (0, 0, 0)  river
--                -- putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) flop" ++) $ show winRatesCalculatedFlop
--                -- putStrLn $ ("Results from MONTE CARLO  Expected Value flop" ++) $ show $ expectedValueIsh winRatesCalculatedFlop ( wouldWin ,  wouldLose )--((read wouldWin :: Int), (read wouldLose :: Int))
--                -- putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) flop" ++) $ show winRatesCalculatedRiver
--                -- putStrLn $ ("Results from MONTE CARLO  Expected Value flop" ++) $ show $ expectedValueIsh winRatesCalculatedRiver (wouldWin , wouldLose)
            

--       _ -> do

               
               
               
--                --Do you need enemyDeckLength?
      
--       -- _ -> do
--       --          let communityCardsFlop = makeDeckFromCommmunity (drop 8 args)
--       --          let newDeck = removeCardsFromDeck d2 communityCardsFlop

--       --          let myCardsPluscommunityCardsFlop = communityCardsFlop ++ myCards

--       --          let clearnedUpWorthyEnenmyStartingHands = filter (\[a,b] -> not $ (a `elem` myCardsPluscommunityCardsFlop) || (b `elem` myCardsPluscommunityCardsFlop) ) worthyEnenmyStartingHands
--       --          let enemyDeckLength = (length clearnedUpWorthyEnenmyStartingHands) -30
--       --          --Do you need enemyDeckLength?
--       --          newRes <-  monteCarloProvidingTheFlop myCards newDeck clearnedUpWorthyEnenmyStartingHands communityCardsFlop enemyDeckLength 20
               
--       --          let flop = [aaa | (aaa,_ )<- newRes ]
--       --          let river = [bbb | (_,  bbb )<- newRes]
--       --          let winRatesCalculatedFlop =  showWinRates $ foldr  xxxxxx   (0, 0, 0)  flop
--       --          let winRatesCalculatedRiver =  showWinRates $ foldr  xxxxxx  (0, 0, 0)  river
--       --          putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) flop" ++) $ show winRatesCalculatedFlop
--       --          putStrLn $ ("Results from MONTE CARLO  Expected Value flop" ++) $ show $ expectedValueIsh winRatesCalculatedFlop ( wouldWin ,  wouldLose )--((read wouldWin :: Int), (read wouldLose :: Int))
--       --          putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%) flop" ++) $ show winRatesCalculatedRiver
--       --          putStrLn $ ("Results from MONTE CARLO  Expected Value flop" ++) $ show $ expectedValueIsh winRatesCalculatedRiver (wouldWin , wouldLose)


--       -- 3 ->
--       -- 4 ->
--    -- --putStrLn $ "\n---   " ++ (show d2 ) ++"\n"

--    -- Now calculate all 3 cards with combinatin of two cards
--    -- About 21187600 combinations
-- --    let allTwoPa = (getAllCombinationsWithTwoPair d2)
-- -- --    let enemyCard = (head allTwoPa)

-- -- --    let allflops = getAllFlopCombinations (removeCards'  d2 enemyCard)
-- -- --    let hmmmmm = map (\x  ->  winnerByPairs  x myCards enemyCard) allflops
-- -- --    putStrLn $ show $ allflops
-- -- --    putStrLn $ show $ hmmmmm
-- -- --    putStrLn $ show $ foldr addTriples (0,0,0)  hmmmmm
   

--    --THIS IS FOR ALL COMBINATIONS OF TWO PAIRS
--    --putStrLn $   "REsults " ++ (show $ solveLazilyish myCards d2 (getAllCombinationsWithTwoPair d2))  --ATTEMPT # ? Not sure, but
--    -- let results = solveLazilyish' myCards d2 (getAllCombinationsWithTwoPair d2)
--    -- putStrLn $ "Results" ++ (show $  results)

--    --THIS is for just worthy enemy hands

--    --eStartingCards <- shuffle worthyEnenmyStartingHands
--    --worthyEnenmyStartingHands' <- shuffle' worthyEnenmyStartingHands

--    -- let hmmm = (\(a1, a2, a3) -> (a1 /n , a2 / n , a3 / n))
--    --putStrLn $ ("Results from MONTE CARLO  " ++)$  show $ (\(a1, a2, a3)-> (fromIntegral a1) / (fromIntegral (a1 + a2 + a3) ))   $ foldr  xxxxxx  (0, 0, 0)  newRes
   
--    -- newRes <- monteCarloSampling myCards d2 worthyEnenmyStartingHands enemyDeckLength 20
--    -- let winRatesCalculated =  showWinRates $ foldr  xxxxxx  (0, 0, 0)  newRes
--    -- putStrLn $ ("Results from MONTE CARLO  (Win%/loss%/Tie%)" ++) $ show winRatesCalculated
--    -- putStrLn $ ("Results from MONTE CARLO  Expected Value" ++) $ show $ expectedValueIsh winRatesCalculated ((read wouldWin :: Int), (read wouldLose :: Int))


--    -- let results = solveLazilyish' myCards d2 (worthyEnenmyStartingHands)
--    -- putStrLn $ "Results" ++ (show $  results)

-- ------------------------------------------------------
-- --FLOP AND RIVER calculation
   





-- ------------------------------------------------------
-- -- Old flop Solver-- First one I made
-- -----------------------------------------------------
--    -- let results = solveLazilyish' myCards d2 (worthyEnenmyStartingHands)
--    -- putStrLn $ "Results" ++ (show $  results)
--    -- putStrLn $ (" Probs winning " ++) $ show $ showWinRates results





-- --    putStrLn $ show $ 
-- --    putStrLn $ show $ )
--    --let flopsLegnth = round $ (fromIntegral $ length allTwoPa) / 10.0
--    --let allCombinationFlopsRelatedToCards =  map (\enenyCards -> getAllFlopCombinations (removeCards'  d2 enenyCards)) (take flopsLegnth allTwoPa)
--    --let enemyCardsPairedWIthAllFlopCombos = zip (take flopsLegnth allTwoPa)  allCombinationFlopsRelatedToCards
--    --let allFlopsByTwoCards = map (\(en, awllFlops)  ->  foldr addTriples (0,0,0) $ map  (\jjj -> winnerByPairs  jjj myCards en) awllFlops) enemyCardsPairedWIthAllFlopCombos
--    --putStrLn $ show $ foldr addTriples (0,0,0) $ allFlopsByTwoCards

--    --let results = foldr addTriples (0,0,0) $ allFlopsByTwoCards

--     --All two paris
--     --

--     --OOOO I could do it this way
--     --Create a two pair 

--    --let results = foldr addTriples (0,0,0) $  map (\(en, awllFlops)  ->  foldr addTriples (0,0,0) $ map  (\jjj -> winnerByPairs  jjj myCards en) awllFlops) $  zip (allTwoPa)    $ map (getAllFlopCombinations . (removeCards'  d2)) allTwoPa




-- --    let results =  calcPairs myCards enemyCard  d2 
-- --    --let results = allCalc myCards (getAllCombinationsWithTwoPair d2) d2
-- --    putStrLn $ show $ getPairs [('a', 2), ('a', 3), ('a', 4), ('b', 2)] [ ('a', 4), ('b', 5)]
-- --    putStrLn $ show $ winnerByPairs   [('a', 2), ('a', 3), ('a', 4), ('b', 2)] [ ('a', 4), ('b', 5)]  [ ('a', 11), ('b', 9)] 
     

-- --    let allTwoPairs =  getAllCombinationsWithTwoPair d2
-- --    let opponentCards = (head allTwoPairs)

-- --    let d3 = removeCards' d2 opponentCards
   

   
-- --    let flops   = getAllNCombinations d3 3
-- -- --    (putStrLn . show . length) flops
-- -- --    let flopsLegnth = round $ (fromIntegral $ length flops) / 100.0
-- -- --    let smallerFlop = take flopsLegnth flops
   
-- --    let results = foldr addTriples (0,0,0)  $ map (\x  ->  winnerByPairs x myCards opponentCards) smallerFlop   
--    --putStrLn $ show results
   
--     --  foldr (addTriples) (0,0,0) $ 
--     --So we need a counnt of who wins, 
--     --So out of amount
--     --Ties as well
--     --putStrLn "Probs"
-- --    putStrLn $   "REsults " ++ (show results)

--    --putStrLn $  "Probs of winning" ++ (show $  results)
--    --putStrLn $  "Probs of Losing" ++ (show $ (\(a1, a2, a3)-> (fromIntegral a2) / (fromIntegral (a1 + a2 + a3) )) results)
--    --putStrLn $  "Probs of tie-ing" ++ (show $ (\(a1, a2, a3)-> (fromIntegral a3) / (fromIntegral (a1 + a2 + a3)) ) results)
   



--     --Then go head to head for parirs


   


--    --allCombs xs = [1..] >>= \n -> mapM (const xs) [1..n] -- https://stackoverflow.com/questions/21775378/get-all-possible-combinations-of-k-elements-from-a-list 

   
--    --putStrLn $ show $ getRandomCardFromDeck2 (getRandomCardFromDeck2 (makeDec, 54), 53)
   

--    -- Given two cards, odds of winning vs random two cards
--    -- 

-- -- Okay the next thing to test is two pair
   


   
--    -- hspecWith (configAddFilter predicate defaultConfig) Spec.spec
--    -- return ()
--    -- https://www.fpcomplete.com/blog/2014/03/monte-carlo-haskell/
--    -- let cnt = 10000000
--    -- successes <- sourceRandomN cnt $$ lengthIfC (\(x, y) -> x*x + y*y < 1)
--    -- print $ successes / cnt * 4


   
--    -- eStartingCards <- shuffle worthyEnenmyStartingHands
--    -- let enemyDeckLength = (length eStartingCards) -30
--    -- newRes <- forM [1..n] $ \i -> do
--    --    sliceer  <- generate  $  (choose (0, enemyDeckLength) :: Gen Int)
      
--    --    solveLazilyish' myCards d2 ( take 30  $  drop sliceer worthyEnenmyStartingHands)




   

-- -----------------------------------------------
-- --------- OLD COMMENTS I HAVE A HARD TIME GETTING RID OF
-- ------------------------------------------------

--    -- putStrLn $ (show makeDec) ++"\n"
--    -- putStrLn $ (show $  removeCard makeDec 'H' 3 ) ++ "\n"
--    -- putStrLn $ (show $ map  (countFiveInRow 1 1)  [[1,2,3,4,5], [2,3,4,4,4,4,4] ]) 
 
--    -- ggg  <- generate  $  (choose (0, 20) :: Gen Int)
--    -- deck <- generate  $  (choose (1, 4) :: Gen Int)
--    -- let suite = transFormToSuite deck
   

--    --putStrLn    $ show  $  filter (\[(a ::Char, b :: Int), (c ::Char, d :: Int)] -> b == d ) $ getAllCombinationsWithTwoPair $ makeDec'  makeDec
--    -- putStrLn $ ("\n" ++ ) $ show $ removeCard  makeDec  suite  ggg  
--    -- putStrLn $ show (suite, ggg)
--    -- deck2 <- getRandomCardFromDeck2 (makeDec, 54)
--    -- putStrLn $ show $ (getAllCombinationsWithTwoPair . makeDec' ) makeDec
--    -- putStrLn $ "\n\n\n\nshow"
--    -- putStrLn $ show  worthyEnenmyStartingHands
--    -- --strings <- readStrings
--    -- s1 <- getLine
--    -- n1 <- getLine
--    -- s2 <- getLine
--    -- n2 <- getLine

--    -- wouldWin  <- getLine
--    -- wouldLose <- getLine
--    --let myCards = [((head s1), (read n1 :: Int)), ((head s2), (read n2 :: Int))]