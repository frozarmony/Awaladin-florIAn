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
		writePlayersTurn(0).
    %.....

    displayGameState([[Score1,Score2], [Board1,Board2], 1])	:-
		write('---------------------------------------'), write('\n'),
		subDisplayGameState('J1', Score1, Board1, lTr), write('\n'),
		subDisplayGameState('J2', Score2, Board2, rTl), write('\n'),
		write('\n'),
		writePlayersTurn(1).
    %.....

	subDisplayGameState(NameJ, ScoreJ, BoardJ, DirectionBoard) :-	ScoreJ > 9,
		writeScore,
        write(NameJ),
        write(' : '),
        write(ScoreJ),
        write(' :- '),
        displayBoard(BoardJ, DirectionBoard),
        !.
    %.....

	subDisplayGameState(NameJ, ScoreJ, BoardJ, DirectionBoard) :-
		writeScore,
        write(NameJ),
        write(' :  '),
        write(ScoreJ),
        write(' :- '),
        displayBoard(BoardJ, DirectionBoard).
    %.....

%-------
%displayBoard(PlayerBoard, Direction)
%-------
	displayBoard([], _)	:-
        write('|'),
        !.
    %.....

	displayBoard([T|Q], rTl) :-
        T > 9,
        write('|'),
        write(T),
        displayBoard(Q, rTl),
        !.
    %.....

	displayBoard([T|Q], rTl) :-
        write('| '),
        write(T),
        displayBoard(Q, rTl).
    %.....

	displayBoard([T|Q], lTr) :-
        T > 9,
        displayBoard(Q, lTr),
        write(T),
        write('|'),
        !.
    %.....

	displayBoard([T|Q], lTr) :-
        displayBoard(Q, lTr),
        write(' '),
        write(T),
        write('|').
		
%-------
%displayEndOfGame([GameStates], PlayerState)
%-------
	displayEndOfGame([[[Score1,Score2],_,_]|_], _)	:-
        Score1 > Score2,
        nl,
        writeWinner(0),
        write(Score1),
        write(' - '),
        write(Score2),
        !.
    %.....

	displayEndOfGame([[[Score1,Score2],_,_]|_], _)	:-
        Score1 < Score2,
        nl,
        writeWinner(1),
        write(Score1),
        write(' - '),
        write(Score2),
        !.
    %.....

	displayEndOfGame([[[Score1,Score2],_,_]|_], _)	:-
        nl,
        writeDraw,
        write(Score1),
        write(' - '),
        write(Score2).

%*****************%
%*	 TRANSLATE   *%
%*****************%

%-------
%writeWrongGameState
%-------
    writeWrongGameState :-
        write('Malformed Initial GameState!!!').

%-------
%writeEmptyBoard
%-------
    writeEmptyBoard :-
        write('Let\'s empty the board !\n').

%-------
%writeWinner(Player)
%-------
    writeWinner(0) :-
        write('J1 is the winner! ').
    %.....

    writeWinner(1) :-
        write('J2 is the winner! ').

%-------
%writePlayersTurn(PlayerTurn)
%-------
    writePlayersTurn(0) :-
        write('J1\'s turn.\n').
    %.....

    writePlayersTurn(1) :-
        write('J2\'s turn.\n').
%-------
%writeScore
%-------
    writeScore :-
        write('Score ').

%-------
%writeDraw
%-------
    writeDraw :-
        write('Draw !').

%-------
%writePossibleActions
%-------
        writePossibleActions :-
            write('Possible Actions : ').

%-------
%writeActions
%-------
    writeActions :-
        write('Action ? :').

%-------
%writeSelectionMenu(Player)
%-------
    writeSelectionMenu(0) :-
        write('J1 Selection Menu :\n').
    %.....

    writeSelectionMenu(1) :-
        write('J1 Selection Menu :\n').

%-------
%writeWrongInput
%-------
    writeWrongInput :-
        write('Invalid input, try again!!\n').

%-------
%writePlayerType(PlayerType)
%-------
    writePlayerType(kHuman) :-
        write('Human').
    %.....
    writePlayerType(kAssistedHuman) :-
        write('Assisted Human').
    %.....

    writePlayerType(kComputer) :-
        write('IA').

%-------
%writeChoose
%-------
    writeChoose :-
        write('Choose : ').

%-------
%writeChoosedAction
%-------
    writeChoosedAction :-
        write('Choosed Action : ').


%-------
%writeAdvisedAction
%-------
    writeAdvisedAction :-
        write('Advised Action : ').
%******************%
%*	   Input	  *%
%******************%

%-------
%choosePlayerType(&TypeJ1, &TypeJ2)
%-------	
	choosePlayerType(TypeJ1,TypeJ2) 	:-
		repeat, writeSelectionMenu(0), choosePlayerType(TypeJ1), !,
		repeat, writeSelectionMenu(1), choosePlayerType(TypeJ2), !.
    %.....

	choosePlayerType(TypeJ)				:-
		write('\t1. '), writePlayerType(kHuman), nl,
        write('\t2. '), writePlayerType(kAssistedHuman), nl,
        write('\t3. '), writePlayerType(kComputer), nl,
        writeChoose,
		read(Number),
        write('\n'),
        interpretNumber(Number, TypeJ).
    %.....

	interpretNumber(1, kHuman)			:- !.
    %.....

	interpretNumber(2, kAssistedHuman)	:- !.
    %.....

	interpretNumber(3, kComputer)		:- !.
    %.....

	interpretNumber(_, _)				:- writeWrongInput, fail.
	

%-------
%chooseAction(GameState, PlayerState, [PossibleActions], &ChoosedAction)
%-------
    chooseAction(_,_, [], 0) :-
        writeEmptyBoard,
        !.
    %.....

	chooseAction([Boards,Scores,PlayerTurn], PlayerState, PossibleActions, ChoosedAction) :-
        PlayerIndex is PlayerTurn+1,
        elementInListAtIndex(PlayerState, PlayerIndex, [HoCPlayer|_]),
        humanOrComputerAction([Boards,Scores,PlayerTurn], HoCPlayer, PossibleActions, ChoosedAction),
        writeChoosedAction,
        write(ChoosedAction), nl.
    %.....
	
%-------
%humanOrComputerAction(GameState, HoCPlayer, [PossibleActions], &ChoosedAction)
%-------
	humanOrComputerAction(GameState, kComputer, PossibleActions, ChoosedAction) :-
        getBestAction(GameState, PossibleActions, ChoosedAction),
        !.
    %.....

	humanOrComputerAction(GameState, kAssistedHuman, PossibleActions, ChoosedAction) :-
        getBestAction(GameState, PossibleActions, AdvisedAction),
        writeAdvisedAction,
        write(AdvisedAction),
        nl,
        askAction(PossibleActions, ChoosedAction),
        !.
    %.....

	humanOrComputerAction(_, kHuman, PossibleActions, ChoosedAction) :-
        askAction(PossibleActions, ChoosedAction),
        !.
	
%-------
%askAction([PossibleActions], &ChoosedAction)
%-------
	askAction(PrePossibleActions, ChoosedAction) :-
		nbFields(Nb),
        zeroOneListToIndex(PrePossibleActions, PossibleActions),
        writePossibleActions,
		write(PossibleActions),
        nl,
        repeat,
		writeActions,
		read(ChoosedAction),
		ChoosedAction > 0,
		ChoosedAction < Nb+1,
		elementInListAtIndex(PrePossibleActions, ChoosedAction, Value),
		Value is 1,
		!.
	
