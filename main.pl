% Awaladin-FlorIAn

%*******************%
%*     Includes    *%
%*******************%

:- include('action.pl').
:- include('IA.pl').
:- include('io.pl').
:- include('tools.pl').

%******************%
%*      MAIN      *%
%******************%

%-------
%awale()
%-------
	awale :- awale([[0,0],[[4,4,4,4,4,4],[4,4,4,4,4,4]],0]).	% Default GameState
	awale(GameStateInit) :-
		init(GameStateInit, PlayerState),
		gameLoop([GameStateInit], PlayerState, [GameState|GameStates]),
        displayGameState(GameState),
		displayEndOfGame([GameState|GameStates], PlayerState),
        clearSearchTree(_),
		!.

%-------
%init(GameState,&PlayerState)
%-------
	init(GameState, [[TypeJ1],[TypeJ2]]) :-
		% Flush Old Conf & Old datas
		retractall(totalSeeds(_)),
		retractall(nbFields(_)),
		retractall(gameLoopState(_)),
		clearSearchTree(_),
		
		% Deduce conf from initGameState
		getParamConf(Fields1, Fields2, TotalSeeds, NbFields),
		asserta(totalSeeds(TotalSeeds)),
		asserta(nbFields(NbFields)),
		choosePlayerType(TypeJ1,TypeJ2).
	
%-------
%getParamConf(Fields1, Fields2, &TotalSeeds,&NbFields)
%-------	
	getParamConf([], [], 0, 0) :- !.
	getParamConf([Field1|Fields1], [Field2|Fields2], TotalSeeds, NbFields) :-
		getParamConf(Fields1, Fields2, SubTotalSeeds, SubNbFields),
		TotalSeeds is Field1 + Field2 + SubTotalSeeds,
		NbFields is SubNbFields + 1,
		!.
	getParamConf(_, _, _, _) :- fail.

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
