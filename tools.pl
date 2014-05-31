%Tools

%-------
%notZeroList(List)
%YES if and only if the list is not made of zeros
%-------
	notZeroList([T|_]) :- T =\= 0, !.
	%.....
	notZeroList([_|Q]) :- notZeroList(Q).
	
%-------
%heaviside(X,Y, &Z)
%Z = 0 if X < Y, Z = 1 if X >= Y
%-------	
	heaviside(X,Y,0) :- X < Y, !.
	heaviside(X,Y,1).

	
%-------
%elementInListAtIndex(List, Index, &Element)
%-------
	elementInListAtIndex([X|L],1,X) :- !.
	elementInListAtIndex([X|L], I, Element) :- I2 is I-1, elementInListAtIndex(L,I2,Element).
	
%-------
%replaceElementInListAtIndexWithElement(List, Index, Element, &NewList)
%-------
	replaceElementInListAtIndexWithElement([T|Q], 1, X, [X|Q]) :- !.
	replaceElementInListAtIndexWithElement([T|Q], Index, X, [T|Q2]) :- NewIndex is Index-1, replaceElementInListAtIndexWithElement(Q, NewIndex, X, Q2).
	
%-------
%listContainsElement(List, Element)
%-------
	listContainsElement([X|_],X).
	listContainsElement([Y|L],X) :- listContainsElement(L,X).

%-------
%getPlayerScore(GameState, &PlayerScore)
%-------
	getPlayerScore([Scores,_,PlayerTurn], PlayerScore) :- NewPlayerTurn is (PlayerTurn+1), elementInListAtIndex(Scores, NewPlayerTurn, PlayerScore).

%-------
%getPlayerBoard(GameState, &PlayerBoard)
%-------

	getPlayerBoard([_,Boards,PlayerTurn], PlayerBoard) :- NewPlayerTurn is (PlayerTurn+1), elementInListAtIndex(Boards, NewPlayerTurn, PlayerBoard).
	

%-------
%getEnemyBoard(GameState, &EnemyBoard)
%-------
	getEnemyBoard([_,Boards,PlayerTurn], EnemyBoard) :- EnemyTurn is (PlayerTurn+1) mod 2, getPlayerBoard([_,Boards, EnemyTurn], EnemyBoard).

%-------
%fieldIndexToPlayerIndex(Field ,&PlayerIndex)
%-------
	fieldIndexToPlayerIndex(Field, PlayerIndex) :- PlayerIndex is (Field-1) div 6.

%-------
%notEndOfGame(GameState)
%-------
	notEndOfGame([[[ScorePlayer1, ScorePlayer2], _, _]|_]) :- ScorePlayer1 < 25, ScorePlayer2 < 25, ScorePlayer1+ScorePlayer2 < 48.


%-------
%chooseAction(GameState, PlayerState, [PossibleActions], &ChoosedAction)
%-------
	chooseAction([Boards,Scores,PlayerTurn], PlayerState, PossibleActions, ChoosedAction) :- PlayerIndex is PlayerTurn+1, elementInListAtIndex(PlayerState, PlayerIndex, [HoCPlayer|_]), humanOrComputerAction([Boards,Scores,PlayerTurn], HoCPlayer, PossibleActions, ChoosedAction).
	
%-------
%humanOrComputerAction(GameState, HoCPlayer, [PossibleActions], &ChoosedAction)
%-------
	humanOrComputerAction(GameState, kComputer, PossibleActions, ChoosedAction) :- write('Computer'), ChoosedAction is 1, !.
	humanOrComputerAction(GameState, kAssistedHuman, PossibleActions, ChoosedAction) :- write('Assisted Human'), askAction(PossibleActions, ChoosedAction),!.
	humanOrComputerAction(GameState, kHuman, PossibleActions, ChoosedAction) :- write('Human'), askAction(PossibleActions, ChoosedAction),  !.
	

%-------
%askAction([PossibleActions], &ChoosedAction)
%-------
	askAction(PossibleActions, ChoosedAction) :- write('Actions possibles : '), write(PossibleActions), nl, repeat, write('Action ? : '), read(ChoosedAction), ChoosedAction > 0, ChoosedAction < 7, elementInListAtIndex(PossibleActions, ChoosedAction, Value), Value is 1, !.
	
%-------
%getPossibleActions(GameState, &[PossibleActions])
%-------
	getPossibleActions(GameState, PossibleActions) :- actionsWithoutFieldEmpty(GameState, [1,1,1,1,1,1], PrePossibleActions), actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions,  PossibleActions).

%-------
%actionsWithoutFieldEmpty(GameState, PrePossibleActions, PossibleActions)
%-------
	actionsWithoutFieldEmpty(GameState, PrePossibleActions, PossibleActions) :- getPlayerBoard(GameState, PlayerBoard), actionsWithoutFieldEmptyInBoard(PlayerBoard, PrePossibleActions, PossibleActions).

%-------
%actionsWithoutFieldEmptyInBoard(Board, PrePossibleActions, PossibleActions)
%-------
	actionsWithoutFieldEmptyInBoard([_|Fields], [0|PrePossibleActions], [0|PossibleActions]) :- actionsWithoutFieldEmptyInBoard(Fields, PrePossibleActions, PossibleActions), !.
	actionsWithoutFieldEmptyInBoard([0|Fields], [_|PrePossibleActions], [0|PossibleActions]) :- actionsWithoutFieldEmptyInBoard(Fields, PrePossibleActions, PossibleActions), !.
	actionsWithoutFieldEmptyInBoard([_|Fields], [_|PrePossibleActions], [1|PossibleActions]) :- actionsWithoutFieldEmptyInBoard(Fields, PrePossibleActions, PossibleActions).
	actionsWithoutFieldEmptyInBoard([], [], []).

%-------
%actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions, &[PossibleActions])
%-------
	actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions,PrePossibleActions) :- getEnemyBoard(GameState, EnemyBoard), enemyBoardIsNotEmpty(EnemyBoard), !.
	actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions, PossibleActions) :- getPlayerBoard(GameState, PlayerBoard), actionsFeedEnemy(PlayerBoard, PrePossibleActions, PossibleActions), notZeroList(PossibleActions).

%-------
%actionsFeedEnemy(PlayerBoard, [PrePossibleActions], &[PossibleActions])
%-------
	actionsFeedEnemy(PlayerBoard, PrePossibleActions, PossibleActions) :- actionsFeedEnemy(PlayerBoard, PrePossibleActions, 1, PossibleActions).
	
%-------
%actionsFeedEnemy(PlayerBoard, [PrePossibleActions], FieldIndex, &[PossibleActions])
%-------
	actionsFeedEnemy([Field|Fields], [0|PrePossibleActions], FieldIndex, [0|PossibleActions] ) :- NewFieldIndex is FieldIndex + 1, actionsFeedEnemy(Fields, PrePossibleActions, NewFieldIndex, PossibleActions).
	actionsFeedEnemy([Field|Fields], [1|PrePossibleActions], FieldIndex, [PossibleAction|PossibleActions]) :- LastField is Field+FieldIndex, heaviside(LastField, 7, PossibleAction), NewFieldIndex is FieldIndex + 1, actionsFeedEnemy(Fields, PrePossibleActions, NewFieldIndex, PossibleActions).
	actionsFeedEnemy([], [], _, []).


	
%******************%
%*	   Display	  *%
%******************%

%-------
%displayGameState(GameState)
%-------
	displayGameState([[Score1,Score2], [Board1,Board2], 0])	:-
		write('---------------------------------------'), write('\n'),
		subDisplayGameState('J2', Score2, Board2, lTr), write('\n'),
		subDisplayGameState('J1', Score1, Board1, rTl), write('\n'),
		write('\n'),
		write('J1\'s turn.\n').
		
	displayGameState([[Score1,Score2], [Board1,Board2], 1])	:-
		write('---------------------------------------'), write('\n'),
		subDisplayGameState('J1', Score1, Board1, lTr), write('\n'),
		subDisplayGameState('J2', Score2, Board2, rTl), write('\n'),
		write('\n'),
		write('J2\'s turn.\n').
		
	subDisplayGameState(NameJ, ScoreJ, BoardJ, DirectionBoard) :-	ScoreJ > 9,
		write('Score '), write(NameJ), write(' : '), write(ScoreJ), write(' :- '), displayBoard(BoardJ, DirectionBoard), !.
		
	subDisplayGameState(NameJ, ScoreJ, BoardJ, DirectionBoard) :-
		write('Score '), write(NameJ), write(' :  '), write(ScoreJ), write(' :- '), displayBoard(BoardJ, DirectionBoard).

%-------
%displayBoard(PlayerBoard, Direction)
%-------
	displayBoard([], _)			:- write('|'), !.
	displayBoard([T|Q], rTl)	:- T > 9, write('|'), write(T), displayBoard(Q, rTl), !.
	displayBoard([T|Q], rTl)	:- write('| '), write(T), displayBoard(Q, rTl).

	displayBoard([T|Q], lTr)	:- T > 9, displayBoard(Q, lTr), write(T), write('|'), !.
	displayBoard([T|Q], lTr)	:- displayBoard(Q, lTr), write(' '), write(T), write('|').
