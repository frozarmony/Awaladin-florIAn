%TOOLS

% Get Current Rank
getCurrentRank(Id, Rank) :- currentRank(Id, Rank), !.
getCurrentRank(Id, 0) :- asserta(currentRank(Id, 0)).

% Update Current Rank
updateCurrentRank(Id, Rank) :- retractall(currentRank(Id, _)), asserta(currentRank(Id, Rank)).

%GET BEST ACTION



%CREATE & UPDATE TREE




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