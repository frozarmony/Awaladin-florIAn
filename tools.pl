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
	heaviside(_,_,1).



%-------
%somme(List,Sum)
%-------

somme([X|Q], Sum) :- somme(Q,Sum2), Sum is Sum2+X, !.
somme([],0).

%-------
%elementInListAtIndex(List, Index, &Element)
%-------
	elementInListAtIndex([X|_],1,X) :- !.
	elementInListAtIndex([_|L], I, Element) :- I2 is I-1, elementInListAtIndex(L,I2,Element).
	
%-------
%replaceElementInListAtIndexWithElement(List, Index, Element, &NewList)
%-------
	replaceElementInListAtIndexWithElement([_|Q], 1, X, [X|Q]) :- !.
	replaceElementInListAtIndexWithElement([T|Q], Index, X, [T|Q2]) :-
        NewIndex is Index-1,
        replaceElementInListAtIndexWithElement(Q, NewIndex, X, Q2).


%-------
%min(A, B, &Min)
%-------
    min(A,B, A) :- A =< B, !.
    min(A,B, B) :- B =< A, !.

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
    max(A,B, A) :- A >= B, !.
    max(A,B, B) :- B >= A, !.

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
%getMaxIndexOfList(List, &MaxIndex)
%-------
getMaxIndexOfList([_], 1) :- !.
getMaxIndexOfList([X|Q], MaxIndex) :- getMaxIndexOfList(Q, 1, X, 2, MaxIndex).

%-------
%getMaxIndexOfList(List, ActualMaxIndex, ActualMax, ActualIndex, &MaxIndex)
%-------
getMaxIndexOfList([X|Q], ActualMaxIndex, ActualMax, ActualIndex, MaxIndex) :- max(X, ActualMax, ActualMax), NewActualIndex is ActualIndex+1, getMaxIndexOfList(Q, ActualMaxIndex, ActualMax, NewActualIndex, MaxIndex), !.

getMaxIndexOfList([X|Q], _, ActualMax, ActualIndex, MaxIndex) :- max(X, ActualMax, X), NewActualIndex is ActualIndex+1, getMaxIndexOfList(Q, ActualIndex, X, NewActualIndex, MaxIndex).

getMaxIndexOfList([], ActualMaxIndex, _, _, ActualMaxIndex).

%-------
%getMinIndexOfList(List, &MinIndex)
%-------
getMinIndexOfList([_], 1) :- !.
getMinIndexOfList([X|Q], MinIndex) :- getMinIndexOfList(Q, 1, X, 2, MinIndex).

%-------
%getMinIndexOfList(List, ActualMinIndex, ActualMin, ActualIndex, &MinIndex)
%-------
getMinIndexOfList([X|Q], ActualMinIndex, ActualMin, ActualIndex, MinIndex) :- min(X, ActualMin, ActualMin), NewActualIndex is ActualIndex+1, getMinIndexOfList(Q, ActualMinIndex, ActualMin, NewActualIndex, MinIndex), !.

getMinIndexOfList([X|Q], _, ActualMin, ActualIndex, MinIndex) :- min(X, ActualMin, X), NewActualIndex is ActualIndex+1, getMinIndexOfList(Q, ActualIndex, X, NewActualIndex, MinIndex).

getMinIndexOfList([], ActualMinIndex, _, _, ActualMinIndex).


%-------
%listContainsElement(List, Element)
%-------
	listContainsElement([X|_],X).
	listContainsElement([_|L],X) :- listContainsElement(L,X).

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
	notEndOfGame([[[ScorePlayer1, ScorePlayer2], _, _]|_]) :- totalSeeds(TotalSeeds), ScorePlayer1 < (TotalSeeds div 2)+1, ScorePlayer2 < (TotalSeeds div 2)+1, ScorePlayer1+ScorePlayer2 < TotalSeeds.

%-------
%cyclicGame(GameState, OldGameStates)
%-------
    cyclicGame(GameState, OldGameStates) :- listContainsElement(OldGameStates, GameState).


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
