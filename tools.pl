%Tools

%-------
%notZeroList(List)
%YES if and only if the list is not made of zeros
%-------
	notZeroList([T|_]) :- T =\= 0, !.
	%.....
	notZeroList([_|Q]) :- zeroList(Q).
	
%-------
%elementInListAtIndex(List, Index, &Element)
%-------
	elementInListAtIndex([X|L],1,X) :- !.
	elementInListAtIndex([X|L], I, Element) :- elementInListAtIndex(L,I2,Element), I is I2+1.

%-------
%getPlayerScore(GameState, &PlayerScore)
%-------
	getPlayerScore([Scores,_,PlayerTurn], PlayerScore) :- elementInListAtIndex(Scores, PlayerTurn, PlayerScore).

%-------
%getPlayerBoard(GameState, &PlayerBoard)
%-------
	getPlayerBoard([_,Boards,PlayerTurn], PlayerBoard) :- elementInListAtIndex(Boards, PlayerTurn, PlayerBoard).
