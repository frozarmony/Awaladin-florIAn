% Awaladin-FloIAn

:- include('tools.pl').
:- include('harvest.pl').
:- include('dealSeeds.pl').
%-------
%awale()
%-------
	awale :-
		retractall(gameLoopState(_)),
		init(GameStateInit, PlayerState),
		gameLoop([GameStateInit], PlayerState, [GameState|GameStates]),
        displayGameState(GameState),
		displayEndOfGame([GameState|GameStates], PlayerState), !.

%-------
%init(&GameState,&PlayerState)
%-------
	init([[0,0],[[4,4,4,4,4,4],[4,4,4,4,4,4]],0], [[TypeJ1],[TypeJ2]]) :- choosePlayerType(TypeJ1,TypeJ2).
	
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
%gameLoop([GameStateI],PlayerState,&[GameStates])
%-------
	gameLoop([GameStateI],PlayerState,NewGameStates)	:-
		asserta(gameLoopState([GameStateI])),
		repeat,
		gameLoopState(GameStates),
		gameTurn(GameStates, PlayerState, NewGameStates),
		retractall(gameLoopState(_)),
		asserta(gameLoopState(NewGameStates)),
		\+ notEndOfGame(NewGameStates),
		!.



%-------
%gameTurn([GameStates], PlayerState, &[NewGameState])
%-------
	gameTurn([GameState|OldGameStates], PlayerState, [NewGameState, GameState | OldGameStates])	:-
		displayGameState(GameState),
		getPossibleActions(GameState, PossibleActions),
		chooseAction(GameState, PlayerState, PossibleActions, ChoosedAction),
		doAction(GameState, ChoosedAction, NewGameState), !.
	
	gameTurn([GameState|OldGameStates], PlayerState, [NewGameState, GameState | OldGameStates])	:-
		emptyBoards(GameState, NewGameState).
		
%-------
%displayEndOfGame([GameStates], PlayerState)
%-------
	displayEndOfGame([[[Score1,Score2],_,_]|_], PlayerState)	:- Score1 > Score2, nl, write('J1 is the winner! '), write(Score1), write(' - '), write(Score2), !.
	displayEndOfGame([[[Score1,Score2],_,_]|_], PlayerState)	:- Score1 < Score2, nl, write('J2 is the winner! '), write(Score1), write(' - '), write(Score2), !.
	displayEndOfGame([[[Score1,Score2],_,_]|_], PlayerState)	:- nl, write('Match is NULL! '), write(Score1), write(' - '), write(Score2).
