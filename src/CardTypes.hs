

module CardTypes where




-- One load this in ghci to test this out
-- Once you have that ord figured out, 
--- NOTE YOU HAVEN't TEST THIS OUT ENOUGH
-- I totally forgot that I need to add in high card, just in case there is a tie for these types
-- For example of two people have Triple of the same cards, then it comes down to having the higest card


-- Then start testing it out --- 
-- Then get the straight, straight flush functions working
-- Then set it up for effcient monte carlo simulation
-- Then Try to get working for today


-- SO what do I need to do to test this out in a tournament?
-- Use monteCarlo Simulation for flop, river turn
-- Figure out how to do so

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



