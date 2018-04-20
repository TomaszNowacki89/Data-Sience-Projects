The task includes the predicting model for the results of the internet game. Each player in the game controls the character of one of the four types (type of character does not change through the course of the game). Each of the players has its initial value, which is the "resultant" of the past results, the level of skills and maybe some more, unknown factors. The game is played in rounds, in which each of the players gets points. The database stores the information of the last twenty rounds plus first, initial "zero" round - which represents the initial state. After each of the round the value of the each player is updated, but it is unknown what is the rule of the value change.
The data consist of: 

- participants.csv - columns: id (of the player), type (character type), team (clan of the player)
- performance.csv - columns: id, points (results in each round), value, round



