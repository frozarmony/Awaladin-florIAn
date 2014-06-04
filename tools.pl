%Tools File

%-------
%notZeroList(List)
%YES if and only if the list is not made of zeros
%-------
	notZeroList([T|_]) :- T =\= 0, !.
	%.....
	notZeroList([_|Q]) :- notZeroList(Q).

%-------
%nOneList(N,&List)
%-------
    nOneList(0,[]) :- !.
    nOneList(N,[1|Q]) :- N2 is N-1, nOneList(N2,Q).

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
	replaceElementInListAtIndexWithElement([T|Q], Index, X, [T|Q2]) :-
        NewIndex is Index-1,
        replaceElementInListAtIndexWithElement(Q, NewIndex, X, Q2).


%-------
%min(A, B, &Min)
%-------
    min(A,B, A) :- A < B, !.
    min(A,B, B) :- B < A, !.
    min(A,B,A).

%-------
%getMinOfList(List, &Min)
%-------
getMinOfList([X], X) :- !.
    getMinOfList([X|Q], Min) :- getMinOfList(Q, X, Min).

	
%-------
%getMinOfList(List, ActualMin, &Min)
%-------
    getMinOfList([X], ActualMin, Min) :- min(X, ActualMin, Min), !.
    getMinOfList([X|Q], ActualMin, Min) :- min(X, ActualMin, NewActualMin), getMinOfList(Q, NewActualMin, Min).

	
%-------
%max(A, B, &Max)
%-------
    max(A,B, A) :- A > B, !.
    max(A,B, B) :- B > A, !.
    max(A,B,A).

%-------
%getMaxOfList(List, &Max)
%-------
    getMaxOfList([X], X) :- !.
    getMaxOfList([X|Q], Max) :- getMaxOfList(Q, X, Max).

%-------
%getMaxOfList(List, ActualMax, &Max)
%-------
    getMaxOfList([X|Q], ActualMax, Max) :- max(X, ActualMax, NewActualMax), getMaxOfList(Q, NewActualMax, Max), !.
    getMaxOfList([], ActualMax, ActualMax).

%-------
%getMaxIndexInList(List, &MaxIndex)
%-------
getMaxIndexInList([X], 1) :- !.
getMaxIndexInList([X|Q], MaxIndex) :- getMaxIndexInList(Q, 1, X, 2, MaxIndex).

%-------
%getMaxIndexInList(List, ActualMaxIndex, ActualMax, ActualIndex, &MaxIndex)
%-------
getMaxIndexInList([X|Q], ActualMaxIndex, ActualMax, ActualIndex, MaxIndex) :- max(X, ActualMax, ActualMax), NewActualIndex is ActualIndex+1, getMaxIndexInList(Q, ActualMaxIndex, ActualMax, NewActualIndex, MaxIndex), !.

getMaxIndexInList([X|Q], ActualMaxIndex, ActualMax, ActualIndex, MaxIndex) :- max(X, ActualMax, X), NewActualIndex is ActualIndex+1, getMaxIndexInList(Q, ActualIndex, X, NewActualIndex, MaxIndex).

getMaxIndexInList([], ActualMaxIndex, _, _, ActualMaxIndex).

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
	fieldIndexToPlayerIndex(Field, PlayerIndex) :- nbFields(NbFields), PlayerIndex is (Field-1) div NbFields.

%-------
%notEndOfGame(GameStates)
%-------
	notEndOfGame([[[ScorePlayer1, ScorePlayer2], Boards, PlayerTurn]|GameStates]) :- totalSeeds(TotalSeeds), ScorePlayer1 < (TotalSeeds div 2)+1, ScorePlayer2 < (TotalSeeds div 2)+1, ScorePlayer1+ScorePlayer2 < TotalSeeds, \+ listContainsElement(GameStates, [[ScorePlayer1, ScorePlayer2], Boards, PlayerTurn]).
	
%-------
%getPossibleActions(GameState, &[PossibleActions])
%-------
	getPossibleActions(GameState, PossibleActions) :- nbFields(NbFields), nOneList(NbFields, List), actionsWithoutFieldEmpty(GameState, List, PrePossibleActions), actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions,  PossibleActions).

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

%-------
%zeroOneListToIndex(ZeroOneList, &IndexList)
%-------
    zeroOneListToIndex(ZeroOneList, IndexList) :- zeroOneListToIndex(ZeroOneList, 1, IndexList).

%-------
%zeroOneListToIndex(ZeroOneList, StartingPoint, &IndexList)
%-------

    zeroOneListToIndex([1|QZO], StartingPoint, [StartingPoint|QIndex]) :-
        NewStartingPoint is StartingPoint+1,
        zeroOneListToIndex(QZO, NewStartingPoint, QIndex), !.
    zeroOneListToIndex([0|QZO], StartingPoint, QIndex) :-
        NewStartingPoint is StartingPoint+1,
        zeroOneListToIndex(QZO, NewStartingPoint, QIndex).
    zeroOneListToIndex([], _, []).
