module CardTypes where



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


-- What do I clearly want?
-- 1. I want the odds at flop in a faster manner. Right now it takes too long to really uese
-- 2. 
-- 3. Make the monte carlo simulation quicker ... This is create for post flop, but not pre-flop.
-- 4. 


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
-- Turning cards into polymorphic type  -> actually this might not be a good idea
-- As that would force more thunks at runtime.
-- Maybe everything should be just bytestring


-- Math here for the entire pipe-line
--   50 C 2 = 1225
---  48 C 3 = 17296
---  45 C 2 = 990
----     1225 * 17296 * 990 = 20, 975, 724, 000 => 20 billion 


-- Monte carlo Simulation
-- batch of 30 of 30 =  900 flop stats
-- = 900  * 900  = 890000   


--Figuring out a way to save the calculations before pre-flop
-- Also keeping the program runnning for flop




