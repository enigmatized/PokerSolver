module CardTypes where
-- What do I clearly want?
-- I want the odds at flop
-- Fix this head error
-- Make the monte carlo simulation quicker
-- Add it so it does five cards 
-- 
-- 1. 
--   do the entire hand 
--       Bonus odds at the turn
--       Bonus odds at the river
--   xxx pot odds
--   
-- 2. Add in multiplayers -- That defintely add in a dimension -- either pull from 
-- 3. Add in command line arguements then asking for values at runtime
--            a. ask for number of players
--            b. ask for pre-flop flop, turnn, ect
--            c. 
--            d. ask how strong I think their hand is -- basically a form of range
--            e. 
--
---  Odds after flops with flops as input
---  odds after turn  with  turn as input

-- pre flop you want to know the odds of winning the entire hand
-- but also the odds of you having them beat at the folp, to not fool yourself

-- 

--   

-- Super advance is being to input their range or one of their cards if you have an idea what that might be





-- TODO FOR PROJECT
-- I also should build a spec test for each function.
-- More effcient monte carlos simulation
-- Turning cards into polymorphic type 
-- 


-- Then start testing it out --- 
-- Then set it up for effcient monte carlo simulation 
-- because there is no way this is going to work out well with adding
-- 2 more cards for each flop. 

-- Math here for the entire pipe-line
--   50 C 2 = 1225
---  48 C 3 = 17296
---  45 C 2 = 990
----     1225 * 17296 * 990 = 20, 975, 724, 000 => 20 billion 


-- Monte carlo Simulation
-- batch of 30 of 30 =  900 flop stats
-- = 900  * 900  = 890000   


-- SO what do I need to do to test this out in a tournament tonight?
-- Make sure flop is working
-- Test out why my ties or so high
-- Maybe create spec tests for this to debug this
-- Add in if you are playing with tight or not tight players
-- 


--Figuring out a way to save the calculations before pre-flop
-- Also keeping the program runnning for flop


data HandValue = 
    HighCard        Int
    | Pair          Int
    | TwoPair       (Int, Int)
    | Triple        Int
    | Straight      Int
    | Flush         Int 
    | Fullhouse     (Int, Int)
    | FourOfAKind   Int
    | StraightFlush Int
    deriving (Show, Ord, Eq)



