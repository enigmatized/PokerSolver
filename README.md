# Texas Holdem Poker Solver



Background : Solving the expected value for a given poker hand with a given number of players is not hard.
There are a lot of web apps that do this currently for free.
But frankly, I didn't trust them, and I thought it wasn't accurate to assume the probability of another players hand to be pure-ly random.

As most "tight" players only play with-in a certain "range" of hands.

So I thought it would be better to solve for the probability given a random hand within a "tight" range.

Also I should note I wanted to learn some haskell while practicing some stats.

I will write about this more.
There is a big TODO.
1. WRite up documentation on how to use it a command line tool.
2. Work on putting players in ranges, meaning being able to say 3 opponents, two of them with tight ranges. This is important, because a player that is able to play a hand because they are the big bling(meaning they are sorta playing for free- regardless if they wanted to or not), should not be catagorized as a tight hand.

