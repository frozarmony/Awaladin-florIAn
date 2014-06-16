%IA File



%GET BEST ACTION

%-------
%getBestAction(CurrentGameState, PossibleActions, &BestAction)
%Récupère la meilleure action
%-------
    getBestAction([Scores, Boards, IA_ID ], PossibleActions, BestAction) :-
        CurrentGameState = [Scores, Boards, IA_ID],
        iaBestAction(IA_ID, CurrentGameState, PossibleActions, 0,12, BestAction). %12 est la profondeur de l'IA


%-------
%evaluationFunction(IA_ID, CurrentGameState, &NodeValue)
%Fonction d'évaluation
%-------
    evaluationFunction(0, [[S1,S2], [_,_], _], NodeValue) :-
        NodeValue is S1-S2.
    %.....

    evaluationFunction(1, [[S1,S2], [_,_], _], NodeValue) :-
        NodeValue is S2-S1.

%*************************%
%*	         IA          *%
%*************************%

%-------
%iaBestAction(IA_ID, CurrentGameState, PrePossibleActions, CurrentRank, FinalRank, &BestAction)
%Fonction principale de l'algorithme MINIMAX
%-------
    iaBestAction(IA_ID, CurrentGameState, PrePossibleActions, CurrentRank, FinalRank, BestAction) :-
        totalSeeds(TotalSeeds),
        MTotalSeeds is -1*TotalSeeds,
        iaSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMax,MTotalSeeds,TotalSeeds,NodeValues,_),
        getMaxIndexOfList(NodeValues, BestActionIndex),
        zeroOneListToIndex(PrePossibleActions, PossibleActions),
        elementInListAtIndex(PossibleActions, BestActionIndex, BestAction).

%-------
%iaSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, MinMax, Alpha, Beta, &NodeValues, &NodeValue)
%MINIMAX effectué sur un sous-arbre
%-------
    iaSubTree(IA_ID, CurrentGameState, CurrentRank, CurrentRank, _, _,_, [NodeValue], NodeValue) :-
        evaluationFunction(IA_ID, CurrentGameState, NodeValue),
        !.
    %.....

    iaSubTree(IA_ID, CurrentGameState, CurrentRank, FinalRank, MinMax, Alpha, Beta, NodeValues, NodeValue) :-
        iaSubNodes(IA_ID, CurrentGameState, CurrentRank, FinalRank, MinMax, Alpha, Beta, NodeValues, NodeValue).

%-------
%iaSubNodes(IA_ID, CurrentGameState, CurrentRank, FinalRank, MinMax, Alpha, Beta, &NodeValues, &NodeValue)
%MINIMAX sur les noeuds d'un sous-arbre
%-------
    iaSubNodes(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMin, Alpha, Beta, NodeValues, NodeValue) :-
        getPossibleActions([CurrentGameState], PrePossibleActions),
        zeroOneListToIndex(PrePossibleActions, PossibleActions),
        totalSeeds(TotalSeeds),
        iaPrepareNextSubNodes(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, kMin, TotalSeeds,  Alpha, Beta, NodeValues, NodeValue),
        !.
    %.....

    iaSubNodes(IA_ID, CurrentGameState, CurrentRank, FinalRank, kMax, Alpha, Beta, NodeValues, NodeValue) :-
        getPossibleActions([CurrentGameState], PrePossibleActions),
        zeroOneListToIndex(PrePossibleActions, PossibleActions),
        totalSeeds(TotalSeeds),
        MTotalSeeds is -1*TotalSeeds,
        iaPrepareNextSubNodes(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, kMax, MTotalSeeds, Alpha, Beta, NodeValues, NodeValue).

%-------
%iaPrepareNextSubNodes(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, MinMax, MinMaxValue, Alpha, Beta, &NodeValues, &NodeValue)
%-------
    iaPrepareNextSubNodes(IA_ID, CurrentGameState, [], CurrentRank, FinalRank, MinMax, MinMaxValue, Alpha, Beta, NodeValues, NodeValue) :-
        iaNextSubNodes(IA_ID, CurrentGameState, 0, CurrentRank, FinalRank, MinMax, MinMaxValue, Alpha, Beta, NodeValues, NodeValue),
        !.
    %.....

    iaPrepareNextSubNodes(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, MinMax, MinMaxValue, Alpha, Beta, NodeValues, NodeValue) :-
        iaNextSubNodes(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, MinMax, MinMaxValue, Alpha, Beta, NodeValues, NodeValue).


%-------
%iaNextSubNodes(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, MinMax, MinMaxValue, Alpha, Beta, &NodeValues, &NodeValue)
%-------
    iaNextSubNodes(IA_ID, CurrentGameState, 0, CurrentRank, FinalRank, kMin, MinMaxValue, Alpha, Beta, [NodeValue|NodeValues], MinValue) :-
        iaSubTree(IA_ID, CurrentGameState, FinalRank, FinalRank, kMax, Alpha, Beta, _, NodeValue),
        min(NodeValue, MinMaxValue, NewMinMaxValue),
        iaNextSubNodesCut(IA_ID, CurrentGameState, [], CurrentRank, FinalRank, kMin, NewMinMaxValue, Alpha, Beta, NodeValues, MinValue),
        !.
    %.....


    iaNextSubNodes(IA_ID, CurrentGameState, [], CurrentRank, FinalRank, kMin, MinMaxValue, Alpha, Beta, [_|NodeValues], MinValue) :-
        iaNextSubNodesCut(IA_ID, CurrentGameState, [], CurrentRank, FinalRank, kMin, MinMaxValue, Alpha, Beta, NodeValues, MinValue),
        !.
    %.....

    iaNextSubNodes(IA_ID, CurrentGameState, [PossibleAction|PossibleActions], CurrentRank, FinalRank, kMin, MinMaxValue, Alpha, Beta, [NodeValue|NodeValues], MinValue) :-
        doAction(CurrentGameState, PossibleAction, NewGameState),
        NewCurrentRank is CurrentRank+1,
        iaSubTree(IA_ID, NewGameState, NewCurrentRank, FinalRank, kMax, Alpha, Beta, _, NodeValue),
        min(Beta, NodeValue, NewBeta),
        min(NodeValue, MinMaxValue, NewMinMaxValue),
        iaNextSubNodesCut(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, kMin, NewMinMaxValue, Alpha, NewBeta, NodeValues, MinValue),
        !.
    %.....


    iaNextSubNodes(IA_ID, CurrentGameState, 0, CurrentRank, FinalRank, kMax, MinMaxValue, Alpha, Beta, [NodeValue|NodeValues], MaxValue) :-
        iaSubTree(IA_ID, CurrentGameState, FinalRank, FinalRank, kMin, Alpha, Beta, _, NodeValue),
        max(NodeValue, MinMaxValue, NewMinMaxValue),
        iaNextSubNodesCut(IA_ID, CurrentGameState, [], CurrentRank, FinalRank, kMax, NewMinMaxValue, Alpha, Beta, NodeValues, MaxValue),
        !.
    %.....


    iaNextSubNodes(IA_ID, CurrentGameState, [], CurrentRank, FinalRank, kMax, MinMaxValue, Alpha, Beta, [_|NodeValues], MaxValue) :-
        iaNextSubNodesCut(IA_ID, CurrentGameState, [], CurrentRank, FinalRank, kMax, MinMaxValue, Alpha, Beta, NodeValues, MaxValue),
        !.
    %.....


    iaNextSubNodes(IA_ID, CurrentGameState, [PossibleAction|PossibleActions], CurrentRank, FinalRank, kMax, MinMaxValue, Alpha, Beta, [NodeValue|NodeValues], MaxValue) :-
        doAction(CurrentGameState, PossibleAction, NewGameState),
        NewCurrentRank is CurrentRank+1,
        iaSubTree(IA_ID, NewGameState, NewCurrentRank, FinalRank, kMin, Alpha, Beta, _, NodeValue),
        max(Alpha, NodeValue, NewAlpha),
        max(MinMaxValue, NodeValue, NewMinMaxValue),
        iaNextSubNodesCut(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, kMax, NewMinMaxValue, NewAlpha, Beta, NodeValues, MaxValue).


%-------
%iaNextSubNodesCut(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, MinMax, MinMaxValue Alpha, Beta, &NodeValues, &NodeValue)
%Coupure AlphaBeta
%-------
    iaNextSubNodesCut(_, _, [], _, _, _, MinMaxValue, _, _, [], MinMaxValue) :-
        !.
    %.....

    iaNextSubNodesCut(_, _, _, _, _, _, MinMaxValue, Alpha, Beta, [], MinMaxValue) :-
        min(Beta,Alpha,Beta),
        !.
    %.....


    iaNextSubNodesCut(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, MinMax, MinMaxValue, Alpha, Beta, NodeValues, NodeValue) :-
        iaNextSubNodes(IA_ID, CurrentGameState, PossibleActions, CurrentRank, FinalRank, MinMax, MinMaxValue, Alpha, Beta, NodeValues, NodeValue).

%********************%
%*	   OUTDATED     *%
%********************%

%*************************%
%*	   Generate Tree     *%
%*************************%


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
        write(NodeValues),
        getMaxIndexOfList(NodeValues, BestActionIndex),
        elementInListAtIndex(FatherToSonsActions, BestActionIndex, BestAction).


%-------
%minimaxABSubTree(IA_ID, CurrentGameState, CurrentRank,FinalRank,kMin/kMax, A,B, &NodeValue)
%-------
    minimaxABSubTree(IA_ID, CurrentGameState, CurrentRank, CurrentRank, _, _,_, NodeValue) :- evaluationFunction(IA_ID, CurrentGameState, NodeValue), !.

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


