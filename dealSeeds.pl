%DealSeeds

%Include TOOLS
:- ['tools.pl'].

%-------
%dealSeeds([Boards], PlayerTurn, ChoosedAction, &[NewBoards], &LastField)
%-------
	dealSeeds([Board1,Board2], 0, ChoosedAction, [NewBoard1,NewBoard2], LastField) :-
		computeFirstIndex(ChoosedAction, FirstIndex),
		dealBoardActive(Board1, FirstIndex, TotalDealt, NewBoard1),
		FirstIndex2 is (FirstIndex+6) mod 12,
		dealBoardPassive(Board2, FirstIndex2, TotalDealt, NewBoard2),
		computeLastField(0, ChoosedAction, TotalDealt, LastField), !.
		
	dealSeeds([Board1,Board2], 1, ChoosedAction, [NewBoard1,NewBoard2], LastField) :-
		computeFirstIndex(ChoosedAction, FirstIndex),
		dealBoardActive(Board2, FirstIndex, TotalDealt, NewBoard2),
		FirstIndex2 is (FirstIndex+6) mod 12,
		dealBoardPassive(Board1, FirstIndex2, TotalDealt, NewBoard1),
		computeLastField(1, ChoosedAction, TotalDealt, LastField), !.

%-------
%computeFirstIndex(ChoosedAction, &FirstIndex)
%-------
	computeFirstIndex(ChoosedAction, FirstIndex)	:- FirstIndex is (13 - ChoosedAction) mod 12.

%-------
%dealBoardActive(Board, FirstIndex, &TotalDealt, &NewBoard)
%-------
	dealBoardActive([], _, _, []).
	dealBoardActive([T|Q], 0, T, [0 | SubBoard])				:- dealBoardActive(Q, 1, T, SubBoard).
	dealBoardActive([T|Q], Index, TotalDealt, [C | SubBoard])	:- NextIndex is (Index+1) mod 12, dealBoardActive(Q, NextIndex, TotalDealt, SubBoard), C is T + ((TotalDealt-Index) div 11) + 1.

%-------
%dealBoardPassive(Board, FirstIndex, TotalDealt, &NewBoard)
%-------
	dealBoardPassive([], _, _, []).
	dealBoardPassive(Board, Index, TotalDealt, NewBoard)		:- AddedSeeds is ((TotalDealt-Index) div 11) + 1, subDealBoardPassive(Board, Index, TotalDealt, NewBoard, AddedSeeds).

	subDealBoardPassive(SubB, Index, TotalDealt, SubB, AddedSeeds)				:- AddedSeeds =:= 0, !.
	subDealBoardPassive([T|Q], Index, TotalDealt, [C|SubBoard], AddedSeeds)		:- C is T+AddedSeeds, NextIndex is (Index+1) mod 12, dealBoardPassive(Q, NextIndex, TotalDealt, SubBoard).
	
%-------
%computeLastField(PlayerTurn, ChoosedAction, TotalDealt, &LastField)
%-------
	computeLastField(PlayerTurn, ChoosedAction, TotalDealt, LastField) :- LastField is ((PlayerTurn*6 + ChoosedAction + TotalDealt + TotalDealt div 12 - 1) mod 12) + 1.
	