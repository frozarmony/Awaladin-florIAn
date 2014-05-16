%Harvest

%Include TOOLS
:- ['tools.pl'].

initTestGameState(TestGameState) :- TestGameState = [ [12,14], [ [4,4,4,4,4,4],[4,4,4,4,4,4]], 1].

initTestEnemyBoard(TestEnemyBoard) :- TestEnemyBoard = [3,3,3,2,3,3].

%-------
%harvestSeeds(GameState, LastBoard, LastField, &NewGameState)
%Not Finished
%-------
	harvestSeeds(GameState, LastField, NewGameState) :- getPlayerBoard(GameState, PlayerBoard), getPlayerScore(GameState, Score), harvestBoard(EnemyBoard, LastField, NewEnemyBoard, NewActualScore), enemyBoardIsNotEmpty(NewEnemyBoard), !.
	%.....
	
	harvestSeeds(GameState, _,_, GameState).


%-------
%enemyBoardIsNotEmpty(Board)
%-------
	notZeroList(Board).

%-------
%harvestBoard(Board, LastField, &NewBoard, &ScoreEarned)
%-------
	harvestBoard(Board, LastField, NewBoard, ScoreEarned) :- harvestBoard(Board, LastField, NewBoard, ScoreEarned, Continue).

%-------
%harvestBoard(Board, LastField, &NewBoard, &ScoreEarned, &Continue)
%-------
	harvestBoard([Field|Board], 1, [0|Board], ScoreEarned,yes) :- harvestField(Field, ScoreEarned), !.
	%.....
	
	harvestBoard(Board, 1, Board, 0, no) :- !.
	%.....
	
	harvestBoard([Field|Board], LastField, [NewField|NewBoard], ScoreEarned, NewContinue) :- OffsetLastField is LastField-1, harvestBoard(Board, OffsetLastField, NewBoard, NewScoreEarned, Continue), harvestFieldIfPossible(Field, Continue, NewField, ActualScore, NewContinue), ScoreEarned is ActualScore+NewScoreEarned, !.


%-------
%harvestFieldIfPossible(Field, CanHarvest, &NewField, &ScoreEarned, &Continue)
%-------
	harvestFieldIfPossible(Field, yes, 0, ScoreEarned, yes) :- harvestField(Field, ScoreEarned).
	%.....
	
	harvestFieldIfPossible(Field, yes, Field, 0, no).
	%.....
	
	harvestFieldIfPossible(Field, no, Field, 0, no). 

%-------
%harvestField(Field, &ScoreEarned)
%-------
	harvestField(Field, Field) :- Field >= 2, Field =< 3, !.
