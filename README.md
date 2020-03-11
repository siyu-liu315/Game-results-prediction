# Game results prediction by supervised machine learning 

# Clean Data structure 
column:  matchname，goldtop/mid/bottom diff， kill#，moster-kill# (dummy)，turrettop/mid/bottom destroy number 
row: min 1 :min 60 

# League of Legends
League of Legends, known also in the gaming world as LoL for short, is a massively popular video game created by Riot Games and released in 2009. It is speculated that there are over 80 million active monthly users for League of Legends and 27 million active daily players, this game is massive by any standard. With such a large player base also comes a large fan following for its league games with professional video game players.


# We Are Looking To Predict If A Team Will Win or Lose Their Game
The problem we are looking to solve is correctly predicting a win or lose in their game. To go a bit further we want to create models that can predict if a team will win or lose their game for each minute the game plays on for until minute 60, which very few games go past this time. Each model will use the information from that minute and prior to make the best prediction. In total we should have 60 total models that will help solve our problem of predicting if the game will end in a win or lose for a certain team. 


# The Importance Of Our Predictions And The Solution We Offer
Our predictions will have significant use for multiple reasons. One use would be for gambling. An accurate prediction could allow for better gambling risk mitigation or arbitrage opportunities. Riot Games might want to utilize these models to have a changing winner prediction for the audience. If the audience sees a clear winner they may want to root for the underdog, or it might encourage the competitors to try harder if they think they are losing. This will possibly allow for more entertaining games which will engross the fans. It could also be utilized by teams to help them strategize to win the game. 
