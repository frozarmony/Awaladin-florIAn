%IA File

%DYNAMIC

:- dynamic( gameStatesArc/5 ).

%IA TOOLS

% Get Current Rank
getCurrentRank(IA_ID, Rank) :- currentRank(IA_ID, Rank), !.
getCurrentRank(IA_ID, 0) :- asserta(currentRank(IA_ID, 0)).

% Update Current Rank
updateCurrentRank(IA_ID, Rank) :- retractall(currentRank(IA_ID, _)), asserta(currentRank(IA_ID, Rank)).

% Clear IA searchTree
clearSearchTree(IA_ID) :- retractall(currentRank(IA_ID, _)), retractall(gameStatesArc(IA_ID, _, _, _, _)).

%GET BEST ACTION

%-------
%getBestAction(CurrentGameState, &BestAction)
%-------
    getBestAction([Scores, Boards, IA_ID ], BestAction) :- CurrentGameState = [Scores, Boards, IA_ID], clearSearchTree(IA_ID), generateSearchTree(IA_ID, CurrentGameState, 0, 3), minimaxABBestAction(IA_ID, CurrentGameState, 0, 3, BestAction).


%*************************%
%*	   Generate Tree     *%
%*************************%

%-------
%updateSearchTree(IA_ID, CurrentGameState, RankDepth)
%-------
	updateSearchTree(IA_ID, CurrentGameState, RankDepth) :-
		gameStatesArc(IA_ID, CurrentGameState, Rank, _, _),
		getCurrentRank(IA_ID, CurrentRank),
		cutSearchTree(IA_ID, CurrentGameState),
		FinalRank is Rank + RankDepth,
		generateSearchTree(IA_ID, CurrentGameState, CurrentRank, FinalRank),
		updateCurrentRank(IA_ID, FinalRank),
		!.
	updateSearchTree(IA_ID, InitGameState, RankDepth) :-
		generateSearchTree(IA_ID, InitGameState, 0, RankDepth),
		updateCurrentRank(IA_ID, RankDepth).

%-------
%cutSearchTree(IA_ID, CurrentGameState, )
%-------
	cutSearchTree(IA_ID, CurrentGameState) :-
		markSearchTree(IA_ID, CurrentGameState),
		retractall(gameStatesArc(IA_ID, _, _, _, _)),
		unmarkSearchTree(IA_ID).
	
	markSearchTree(IA_ID, CurrentGameState) :-
		gameStatesArc(IA_ID, CurrentGameState, Rank, SonsGameStates, FatherToSonsActions),
		subMarkSearchTree(IA_ID, SonsGameStates),
		asserta(gameStatesArc(mark, CurrentGameState, Rank, SonsGameStates, FatherToSonsActions)),
		!.
	markSearchTree(_, _).
	
	subMarkSearchTree(_, []) :- !.
	subMarkSearchTree(IA_ID, [SonGameState|SonsGameStates]) :-
		markSearchTree(IA_ID, SonGameState),
		subMarkSearchTree(IA_ID, SonsGameStates).

	unmarkSearchTree(IA_ID) :-
		retract(gameStatesArc(mark, CurrentGameState, Rank, SonsGameStates, FatherToSonsActions)),
		asserta(gameStatesArc(IA_ID, CurrentGameState, Rank, SonsGameStates, FatherToSonsActions)),
		fail.
	unmarkSearchTree(_).

%-------
%generateSearchTree(IA_ID, CurrentGameState, CurrentRank, FinalRank)
%-------
	generateSearchTree(IA_ID, CurrentGameState, 0, _) :- generateGameStatesArc(IA_ID, CurrentGameState, 0), fail.
	generateSearchTree(_, _, CurrentRank, FinalRank) :- CurrentRank >= FinalRank, !.
	generateSearchTree(IA_ID, _, CurrentRank, _) :-
		gameStatesArc(IA_ID, _, CurrentRank, SonsGameStates, _),
		NextRank is CurrentRank + 1,
		generateSonsGameStatesArc(IA_ID, SonsGameStates, NextRank),
		fail.
	generateSearchTree(IA_ID, _, CurrentRank, FinalRank) :-
		NextRank is CurrentRank + 1,
		generateSearchTree(IA_ID, _, NextRank, FinalRank).



	generateSonsGameStatesArc(_, [], _) :- !.
	generateSonsGameStatesArc(IA_ID, [SonGameState|SonsGameStates], SonRank) :-
		generateGameStatesArc(IA_ID, SonGameState, SonRank),
		generateSonsGameStatesArc(IA_ID, SonsGameStates, SonRank).

%-------
%generateGameStatesArc(IA_ID, FatherGameState, FatherRank)
%-------
	generateGameStatesArc(IA_ID, FatherGameState, FatherRank) :-
		getPossibleActions([FatherGameState], PossibleActions),
		generateSonsGameStates(FatherGameState, 1, PossibleActions, SonsGameStates, FatherToSonsActions),
		asserta(gameStatesArc(IA_ID, FatherGameState, FatherRank, SonsGameStates, FatherToSonsActions)).
	
	generateSonsGameStates(_, _, [], [], []) :- !.
	generateSonsGameStates(FatherGameState, CurrentAction, [0|PossibleActions], SonsGameStates, FatherToSonsActions) :-
		NextAction is CurrentAction + 1, generateSonsGameStates(FatherGameState, NextAction, PossibleActions, SonsGameStates, FatherToSonsActions), !.
	generateSonsGameStates(FatherGameState, CurrentAction, [1|PossibleActions], [SonGameState|SonsGameStates], [CurrentAction|FatherToSonsActions]) :-
		doAction(FatherGameState, CurrentAction, SonGameState), NextAction is CurrentAction + 1, generateSonsGameStates(FatherGameState, NextAction, PossibleActions, SonsGameStates, FatherToSonsActions).



%***************************%
%*	   Minimax Algorithm   *%
%***************************%

%-------
%minimaxBestAction(IA_ID, CurrentGameState, CurrentRank,FinalRank,&BestAction)
%-------
    minimaxBestAction(IA_ID, CurrentGameState, CurrentRank, FinalRank, BestAction) :-
        gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, FatherToSonsActions),
        NewCurrentRank is CurrentRank + 1,
        minimaxSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMin,NodeValues),
        getMaxIndexOfList(NodeValues, BestActionIndex),
        elementInListAtIndex(FatherToSonsActions, BestActionIndex, BestAction).


%-------
%minimaxSubTree(IA_ID, CurrentGameState, CurrentRank,FinalRank,kMin/kMax, &NodeValue)
%-------
    minimaxSubTree(IA_ID, CurrentGameState, CurrentRank, CurrentRank, _, NodeValue) :- evaluationFunction(IA_ID, CurrentGameState, NodeValue), !.

    minimaxSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMin, NodeValue) :-
        gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, _),
        NewCurrentRank is CurrentRank + 1,
        minimaxSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMax,NodeValues),
        getMinOfList(NodeValues, NodeValue), !.

    minimaxSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMax, NodeValue) :-
        gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, _),
        NewCurrentRank is CurrentRank + 1,
        minimaxSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMin,NodeValues),
        getMaxOfList(NodeValues, NodeValue), !.

    minimaxSubTree(IA_ID, CurrentGameState, CurrentRank, _, _, NodeValue) :-
        gameStatesArc(IA_ID, CurrentGameState, CurrentRank, [], []),
        evaluationFunction(IA_ID,CurrentGameState, NodeValue), !.


%-------
%minimaxSubNodes(IA_ID, [SonsGameStates], CurrentRank,FinalRank, kMin/kMax, &NodeValues)
%-------
    minimaxSubNodes(IA_ID, [SonGameState|SonsGameStates], CurrentRank, FinalRank, MinMax, [NodeValue|NodeValues]) :-
        minimaxSubTree(IA_ID, SonGameState, CurrentRank, FinalRank, MinMax, NodeValue),
        minimaxSubNodes(IA_ID, SonsGameStates, CurrentRank, FinalRank, MinMax, NodeValues), !.

	minimaxSubNodes(_, [], _, _, _, []).

%***************************%
%*	 MinimaxAB Algorithm   *%
%***************************%

%-------
%minimaxABBestAction(IA_ID, CurrentGameState, CurrentRank,FinalRank,&BestAction)
%-------
    minimaxABBestAction(IA_ID, CurrentGameState, CurrentRank, FinalRank, BestAction) :-
        gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, FatherToSonsActions),
        NewCurrentRank is CurrentRank + 1,
        minimaxABSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMin,-100,100,100,NodeValues,_),
        getMaxIndexOfList(NodeValues, BestActionIndex),
        elementInListAtIndex(FatherToSonsActions, BestActionIndex, BestAction).


%-------
%minimaxABSubTree(IA_ID, CurrentGameState, CurrentRank,FinalRank,kMin/kMax, A,B, &NodeValue)
%-------
    minimaxABSubTree(IA_ID, CurrentGameState, CurrentRank, CurrentRank, _, A,B, NodeValue) :- evaluationFunction(IA_ID, CurrentGameState, NodeValue), !.

    minimaxABSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMin, A,B, NodeValue) :-
        gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, _),
        NewCurrentRank is CurrentRank + 1,
        minimaxABSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMax,A,B,-100,_,NodeValue),
         !.

    minimaxABSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMax,A,B, NodeValue) :-
        gameStatesArc(IA_ID, CurrentGameState, CurrentRank, SonsGameStates, _),
        NewCurrentRank is CurrentRank + 1,
        minimaxABSubNodes(IA_ID, SonsGameStates, NewCurrentRank, FinalRank, kMin,A,B,100,_,NodeValue),
         !.

    minimaxABSubTree(IA_ID, CurrentGameState, CurrentRank, _, _, _,_, NodeValue) :-
        gameStatesArc(IA_ID, CurrentGameState, CurrentRank, [], []),
        evaluationFunction(IA_ID,CurrentGameState, NodeValue), !.


%-------
%minimaxABSubNodes(IA_ID, [SonsGameStates], CurrentRank,FinalRank, kMin/kMax,A,B,PreNodeValue, &NodeValues, &MinMaxValue)
%-------
    minimaxABSubNodes(IA_ID, [SonGameState|SonsGameStates], CurrentRank, FinalRank, kMin, A,B,PreNodeValue, [NodeValue|NodeValues], MinMaxValue) :-
        minimaxABSubTree(IA_ID, SonGameState, CurrentRank, FinalRank, kMin, A,B, NewNodeValue),
        min(NewNodeValue,PreNodeValue,NodeValue),
        minimaxABNextSubNodes(IA_ID, SonsGameStates, CurrentRank, FinalRank, kMin,A,B, NewNodeValue, NodeValues, MinMaxValue),
        !.

    minimaxABSubNodes(IA_ID, [SonGameState|SonsGameStates], CurrentRank, FinalRank, kMax, A,B, PreNodeValue, [NodeValue|NodeValues], MinMaxValue) :-
        minimaxABSubTree(IA_ID, SonGameState, CurrentRank, FinalRank, kMax, A,B, NewNodeValue),
        max(NewNodeValue,PreNodeValue,NodeValue),
        minimaxABNextSubNodes(IA_ID, SonsGameStates, CurrentRank, FinalRank, kMax,A,B, NewNodeValue, NodeValues, MinMaxValue),

        !.


    minimaxABSubNodes(_,[],_,_,_,_,_,PreNodeValue,[],PreNodeValue).



%-------
%minimaxABNextSubNodes(IA_ID, [SonsGameStates], CurrentRank,FinalRank, kMin/kMax,A,B, PreNodeValue, &NodeValues, MinMaxValue)
%-------
minimaxABNextSubNodes(_,[],_,_,kMin,A,_,PreNodeValue,[],PreNodeValue) :-  A >= PreNodeValue.

minimaxABNextSubNodes(IA_ID,SonsGameStates,CurrentRank,FinalRank,kMin,A,B,PreNodeValue, NodeValues,MinMaxValue) :- min(B,PreNodeValue, NewB), minimaxABSubNodes(IA_ID, SonsGameStates, CurrentRank, FinalRank, kMin,A,NewB,PreNodeValue, NodeValues, MinMaxValue).


minimaxABNextSubNodes(_,[],_,_,kMax,_,B,PreNodeValue, [],PreNodeValue) :-  B =< PreNodeValue.

minimaxABNextSubNodes(IA_ID,SonsGameStates,CurrentRank,FinalRank,kMax,A,B,PreNodeValue, NodeValues,MinMaxValue) :- max(PreNodeValue, A, NewA),minimaxABSubNodes(IA_ID, SonsGameStates, CurrentRank, FinalRank, kMax,NewA,B,PreNodeValue, NodeValues, MinMaxValue).




%-------
%evaluationFunction(IA_ID, CurrentGameState, &NodeValue)
%-------
    evaluationFunction(0, [[S1,S2], [B1,B2], _], NodeValue) :-  somme(B1,SumB1), somme(B2,SumB2), NodeValue is S1+SumB1-S2-SumB2, !.
    evaluationFunction(1, [[S1,S2], [B1,B2], _], NodeValue) :- somme(B1,SumB1), somme(B2,SumB2),NodeValue is -1*(S1+SumB1-S2-SumB2), !.

%DATA STRUCTURE

%gameStatesArc(IA_ID, FatherGameState, FatherRank, [SonsGameStates], [FatherToSonsActions])