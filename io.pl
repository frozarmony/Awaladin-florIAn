% Input Output File

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
		
%-------
%displayEndOfGame([GameStates], PlayerState)
%-------
	displayEndOfGame([[[Score1,Score2],_,_]|_], _)	:- Score1 > Score2, nl, write('J1 is the winner! '), write(Score1), write(' - '), write(Score2), !.
	displayEndOfGame([[[Score1,Score2],_,_]|_], _)	:- Score1 < Score2, nl, write('J2 is the winner! '), write(Score1), write(' - '), write(Score2), !.
	displayEndOfGame([[[Score1,Score2],_,_]|_], _)	:- nl, write('Match is NULL! '), write(Score1), write(' - '), write(Score2).

%******************%
%*	   Input	  *%
%******************%

%-------
%choosePlayerType(&TypeJ1, &TypeJ2)
%-------	
	choosePlayerType(TypeJ1,TypeJ2) 	:-
		repeat, write('J1 Selection Menu :\n'), choosePlayerType(TypeJ1), !,
		repeat, write('J2 Selection Menu :\n'), choosePlayerType(TypeJ2), !.
	choosePlayerType(TypeJ)				:-
		write('\t1. Human\n'),write('\t2. Assisted Human\n'),write('\t3. IA\n'), write('Choose : '),
		read(Number), write('\n'), interpretNumber(Number, TypeJ).
		
	interpretNumber(1, kHuman)			:- !.
	interpretNumber(2, kAssistedHuman)	:- !.
	interpretNumber(3, kComputer)		:- !.
	interpretNumber(_, _)				:- write('Invalid input, try again!!\n'), fail.
	

%-------
%chooseAction(GameState, PlayerState, [PossibleActions], &ChoosedAction)
%-------
    chooseAction(_,_, [], 0) :- write('Let\'s empty the board !\n'), !.
	chooseAction([Boards,Scores,PlayerTurn], PlayerState, PossibleActions, ChoosedAction) :- PlayerIndex is PlayerTurn+1, elementInListAtIndex(PlayerState, PlayerIndex, [HoCPlayer|_]), humanOrComputerAction([Boards,Scores,PlayerTurn], HoCPlayer, PossibleActions, ChoosedAction), write('Choosed Action : '), write(ChoosedAction), nl.
	
%-------
%humanOrComputerAction(GameState, HoCPlayer, [PossibleActions], &ChoosedAction)
%-------
	humanOrComputerAction(GameState, kComputer, _, ChoosedAction) :- getBestAction(GameState, ChoosedAction), !.
	humanOrComputerAction(GameState, kAssistedHuman, PossibleActions, ChoosedAction) :- getBestAction(GameState, AdvisedAction), write('Advised Action : '), write(AdvisedAction), nl, askAction(PossibleActions, ChoosedAction),!.
	humanOrComputerAction(_, kHuman, PossibleActions, ChoosedAction) :- askAction(PossibleActions, ChoosedAction),  !.
	
%-------
%askAction([PossibleActions], &ChoosedAction)
%-------
	askAction(PrePossibleActions, ChoosedAction) :-
		nbFields(Nb),
        zeroOneListToIndex(PrePossibleActions, PossibleActions),
        write('Possible Actions : '),
		write(PossibleActions), nl, repeat,
		write('Action ? : '),
		read(ChoosedAction),
		ChoosedAction > 0,
		ChoosedAction < Nb+1,
		elementInListAtIndex(PrePossibleActions, ChoosedAction, Value),
		Value is 1,
		!.
	
