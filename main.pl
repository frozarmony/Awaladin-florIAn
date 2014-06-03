% Awaladin-FloIAn

:- include('action.pl').
:- include('IA.pl').
:- include('io.pl').
:- include('tools.pl').

%-------
%awale()
%-------
	awale :-
		retractall(gameLoopState(_)),
		init(GameStateInit, PlayerState),
		gameLoop([GameStateInit], PlayerState, [GameState|GameStates]),
        displayGameState(GameState),
		displayEndOfGame([GameState|GameStates], PlayerState),
		!.

%-------
%init(&GameState,&PlayerState)
%-------
	init([[0,0],[[4,4,4,4,4,4],[4,4,4,4,4,4]],0], [[TypeJ1],[TypeJ2]]) :- choosePlayerType(TypeJ1,TypeJ2).
	
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
