# Texas Holdem Poker Solver



Background : Solving the expected value for a given poker hand with a given number of players is not hard.
There are a lot of web apps that do this currently for free.
But frankly, I didn't trust them, and I thought it wasn't accurate to assume the probability of another players hand to be pure-ly random.

As most "tight" players only play with-in a certain "range" of hands.

So I thought it would be better to solve for the probability given a random hand within a "tight" range.

I should note another motivation for this project was to learn some haskell while practicing some stats/math.

I will write about this more.
There is a big TODO.
1. WRite up documentation on how to use it a command line tool.
2. Work on putting players in ranges, meaning being able to say 3 opponents, two of them with tight ranges. This is important, because a player that is able to play a hand because they are the big bling(meaning they are sorta playing for free- regardless if they wanted to or not), should not be catagorized as a tight hand.
3. Major code cleaning
   a.  Reduce the amount of functions/duplicate code (IE for flop and given cards should be the same)
4. Better stats, right now I am just averaging the results, but since I am doing batch/monte carlo simulations/bootstrapping that I should use some better stats like t-tests, or even simple SD with some epsilon
5. Optimize the amount of sampling I should do depending on the combintorics problem needed to solve.
     a. Maybe create a command line arguement for the amount of time I can use to solve the problem, which would give me a accurate way of seeing how much I should sample
6. Solve for an optimal betting value? -- As of now I provide one.
7. Add in more complicated values like implied odds
8. Add in a Decision tree element.
9. Solve for hands to beat, vs hands not
10. Write spec tests for the hand solver part- statistically I think I am getting into ties more than I should.
11. -- Stretch goal.. create this for Omaha and then omaha Hi/Low (I beleive there isn't many good solvers for omaha Hi/Low therefore this could have potential on small stakes games online. )
12. Also taking seating into account. Meaning When I am big blind, or who is first to bet

This is a small tool in a larger project, where the goal is to create a fully automated bot to play in multiple online tournaments at once. 

