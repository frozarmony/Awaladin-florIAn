%DealSeeds

%Include TOOLS
:- ['tools.pl'].

%-------
%dealSeeds([Boards], ChoosedAction, &[NewBoards], &LastField)
%-------
	%dealSeeds([Boards], ChoosedAction, &[NewBoards], &LastField)
	% Magic Formule

%-------
%compute(ChoosedAction, &FirstIndex)
%-------
	compute(ChoosedAction, FirstIndex)	:- FirstIndex is (13 - ChoosedAction) mod 12.

%-------
%dealBoard(Board, FirstIndex, &TotalDealt, &NewBoard)
%-------
	dealBoard([], _, _, []).
	dealBoard([T|Q], 0, T, [0 | SubBoard])				:- dealBoard(Q, 1, T, SubBoard).
	dealBoard([T|Q], Index, TotalDealt, [C | SubBoard])	:- NextIndex is (Index+1) mod 12, dealBoard(Q, NextIndex, TotalDealt, SubBoard), C is T+1.	% Magic Formule!!!