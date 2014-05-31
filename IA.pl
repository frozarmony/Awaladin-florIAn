%INCLUDES

:- include('tools.pl').
:- include('main.pl').

%TOOLS

% Get Current Rank
getCurrentRank(IA_ID, Rank) :- currentRank(IA_ID, Rank), !.
getCurrentRank(IA_ID, 0) :- asserta(currentRank(IA_ID, 0)).

% Update Current Rank
updateCurrentRank(IA_ID, Rank) :- retractall(currentRank(IA_ID, _)), asserta(currentRank(IA_ID, Rank)).

% Clear IA searchTree
clearSearchTree(IA_ID) :- retractall(currentRank(IA_ID, _)), retractall(gameStatesArc(IA_ID, _, _, _, _)).

%GET BEST ACTION



%CREATE & UPDATE TREE


%-------
%generateSearchTree(IA_ID, CurrentGameState, CurrentRank, FinalRank)
%-------
	generateSearchTree(IA_ID, CurrentGameState, 0, _) :- generateGameStatesArc(IA_ID, CurrentGameState, 0), fail.
	generateSearchTree(IA_ID, _, CurrentRank, FinalRank) :- CurrentRank >= FinalRank, !.
	generateSearchTree(IA_ID, _, CurrentRank, _) :-
		gameStatesArc(IA_ID, FatherGameState, CurrentRank, SonsGameStates, _),
		NextRank is CurrentRank + 1,
		generateSonsGameStatesArc(IA_ID, SonsGameStates, NextRank),
		fail.
	generateSearchTree(IA_ID, _, CurrentRank, FinalRank) :-
		NextRank is CurrentRank + 1,
		generateSearchTree(IA_ID, _, NextRank, FinalRank).
	
	generateSonsGameStatesArc(IA_ID, [], SonRank) :- !.
	generateSonsGameStatesArc(IA_ID, [SonGameState|SonsGameStates], SonRank) :-
		generateGameStatesArc(IA_ID, SonGameState, SonRank),
		generateSonsGameStatesArc(IA_ID, SonsGameStates, SonRank).

%-------
%generateGameStatesArc(IA_ID, FatherGameState, FatherRank)
%-------
	generateGameStatesArc(IA_ID, FatherGameState, FatherRank) :-
		getPossibleActions(FatherGameState, PossibleActions),
		generateSonsGameStates(FatherGameState, 1, PossibleActions, SonsGameStates, FatherToSonsActions),
		asserta(gameStatesArc(IA_ID, FatherGameState, FatherRank, SonsGameStates, FatherToSonsActions)).
	
	generateSonsGameStates(_, _, [], [], []) :- !.
	generateSonsGameStates(FatherGameState, CurrentAction, [0|PossibleActions], SonsGameStates, FatherToSonsActions) :-
		NextAction is CurrentAction + 1, generateSonsGameStates(FatherGameState, NextAction, PossibleActions, SonsGameStates, FatherToSonsActions), !.
	generateSonsGameStates(FatherGameState, CurrentAction, [1|PossibleActions], [SonGameState|SonsGameStates], [CurrentAction|FatherToSonsActions]) :-
		doAction(FatherGameState, CurrentAction, SonGameState), NextAction is CurrentAction + 1, generateSonsGameStates(FatherGameState, NextAction, PossibleActions, SonsGameStates, FatherToSonsActions).

%MINIMAX ALGORITHM

%-------
%minimaxBestAction(IA_ID, CurrentGameState, CurrentRank,FinalRank,&BestAction)
%-------
    minimaxBestAction(IA_ID, CurrentGameState, CurrentRank, FinalRank, BestAction) :- retract(gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, FatherToSonsActions)), NewCurrentRank is CurrentRank + 1, minimaxSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMin,NodeValues), getMaxIndexInList(NodeValues, MaxIndex), elementInListAtIndex(FatherToSonsActions, MaxIndex, BestAction).


%-------
%minimaxSubTree(IA_ID, CurrentGameState, CurrentRank,FinalRank,kMin/kMax, &NodeValue)
%-------
    minimaxSubTree(IA_ID, CurrentGameState, CurrentRank, CurrentRank, _, NodeValue) :- evaluationFunction(CurrentGameState, NodeValue), !.

    minimaxSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, _, NodeValue) :- retract(gameStatesArc(IA_ID, CurrentGameState, CurrentRank, [], [])), evaluationFunction(CurrentGameState, NodeValue), !.

    minimaxSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMin, NodeValue) :-  retract(gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, FatherToSonsActions)), NewCurrentRank is CurrentRank + 1, minimaxSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMax,NodeValues), getMinOfList(NodeValues, NodeValue).

    minimaxSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMax, NodeValue) :-  retract(gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, FatherToSonsActions)), NewCurrentRank is CurrentRank + 1, minimaxSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMin,NodeValues), getMaxOfList(NodeValues, NodeValue).

%-------
%minimaxSubNodes(IA_ID, [SonsGameStates], CurrentRank,FinalRank, kMin/kMax, &NodeValues)
%-------
    minimaxSubNodes(_, [], _, _, _, []).
    minimaxSubNodes(IA_ID, [SonGameState|SonsGameStates], CurrentRank, FinalRank, MinMax, [NodeValue|NodeValues]) :- minimaxSubTree(IA_ID, SonGameState, CurrentRank, FinalRank, MinMax, NodeValue), minimaxSubNodes(IA_ID, SonsGameStates, CurrentRank, FinalRank, MinMax, NodeValues).


%-------
%evaluationFunction(CurrentGameState, &NodeValue)
%-------


%DATA STRUCTURE

%gameStatesArc(IA_ID, FatherGameState, FatherRank, [SonsGameStates], [FatherToSonsActions])