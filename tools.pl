%Tools

%-------
%notZeroList(List)
%YES if and only if the list is not made of zeros
%-------
	notZeroList([T|_]) :- T =\= 0, !.
	%.....
	notZeroList([_|Q]) :- notZeroList(Q).
	
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
