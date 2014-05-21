%Harvest

%Include TOOLS
:-['tools.pl'].

initTestGameState(TestGameState) :- TestGameState = [ [24,24], [ [0,0,0,0,8,7],[4,5,5,6,6,7]], 0].

initTestPlayerState(TestPlayerState) :- TestPlayerState = [ [kHuman], [kHuman]].


initTestEnemyBoard(TestEnemyBoard) :- TestEnemyBoard = [1,0,3,2,3,3].

%-------
%doAction(GameState, ChoosedAction, &NewGameState)
%-------
	doAction([Scores, Boards, PlayerTurn], ChoosedAction, [NewScores, NewBoards, NewPlayerTurn]) :-
		dealSeeds(Boards, PlayerTurn, ChoosedAction, PreNewBoards, LastField),
		harvestSeeds([Scores, PreNewBoards, PlayerTurn], LastField, [NewScores, NewBoards, PlayerTurn]),
		NewPlayerTurn is (PlayerTurn + 1) mod 2.


%-------
%harvestSeeds(GameState, LastField, &NewGameState)
%-------
	harvestSeeds([Scores, Boards, PlayerTurn], LastField, [NewScores, NewBoards, PlayerTurn]) :- fieldIndexToPlayerIndex(LastField, EnemyPlayer),  PlayerTurn \= EnemyPlayer, getEnemyBoard([_, Boards, PlayerTurn], EnemyBoard), getPlayerScore([Scores, _, PlayerTurn], Score), RelativeLastField is LastField mod 6, harvestBoard(EnemyBoard, RelativeLastField, NewEnemyBoard, NewScore), enemyBoardIsNotEmpty(NewEnemyBoard), PlayerIndex is PlayerTurn +1, NewPlayerScore is Score+NewScore, replaceElementInListAtIndexWithElement(Scores, PlayerIndex, NewPlayerScore , NewScores),  EnemyPlayerIndex is EnemyPlayer +1, replaceElementInListAtIndexWithElement(Boards, EnemyPlayerIndex, NewEnemyBoard, NewBoards), !.
	%.....
	
	harvestSeeds(GameState,_, GameState).


%-------
%enemyBoardIsNotEmpty(Board)
%-------
	enemyBoardIsNotEmpty(Board) :- notZeroList(Board).

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

%-------
%emptyBoards(GameState, &NewGameState)
%-------
	emptyBoards([[], [], _], [[], [], _]) :- !.
	emptyBoards([[Score|Scores], [Board|Boards], _], [ [NewScore|NewScores], [NewBoard|NewBoards], _]) :- emptyBoards([Scores,Boards,_], [NewScores, NewBoards, _]), emptyBoard(Board, NewBoard, ScoreEarned), NewScore is Score+ScoreEarned.

%-------
%emptyBoard(Board, &NewBoard, &ScoreEarned)
%-------
	emptyBoard([], [], 0) :- !.
	emptyBoard([Field|Fields], [0|NewFields], ScoreEarned) :- emptyBoard(Fields, NewFields, NewScoreEarned), ScoreEarned is NewScoreEarned + Field.
	
