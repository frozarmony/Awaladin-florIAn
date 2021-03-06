% Action File

%-------
%doAction(GameState, ChoosedAction, &NewGameState)
%Effectue l'action choisie
%-------
    doAction([Scores, Boards, PlayerTurn], 0, [NewScores, NewBoards, NewPlayerTurn]) :-
        emptyBoards([Scores, Boards, PlayerTurn], [NewScores, NewBoards, _]),
        NewPlayerTurn is (PlayerTurn + 1) mod 2,
        !.
    %.....

	doAction([Scores, Boards, PlayerTurn], ChoosedAction, [NewScores, NewBoards, NewPlayerTurn]) :-
		dealSeeds(Boards, PlayerTurn, ChoosedAction, PreNewBoards, LastField),
		harvestSeeds([Scores, PreNewBoards, PlayerTurn], LastField, [NewScores, NewBoards, PlayerTurn]),
		NewPlayerTurn is (PlayerTurn + 1) mod 2.

%**************************%
%*	  PossibleActions	  *%
%**************************%

%-------
%getPossibleActions(GameStates, &[PossibleActions])
%Actions possibles sous la forme [0,1,0,0,1,...]
%-------
    getPossibleActions([GameState|OldGameStates], PossibleActions) :-
        \+ cyclicGame(GameState, OldGameStates),
        nbFields(NbFields), nOneList(NbFields, List),
        actionsWithoutFieldEmpty(GameState, List, PrePossibleActions),
        actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions,  PossibleActions),
        !.
    %.....

    getPossibleActions(_, []).

%-------
%actionsWithoutFieldEmpty(GameState, PrePossibleActions, PossibleActions)
%Actions non nulles
%-------
    actionsWithoutFieldEmpty(GameState, PrePossibleActions, PossibleActions) :-
        getPlayerBoard(GameState, PlayerBoard),
        actionsWithoutFieldEmptyInBoard(PlayerBoard, PrePossibleActions, PossibleActions).

%-------
%actionsWithoutFieldEmptyInBoard(Board, PrePossibleActions, PossibleActions)
%-------
    actionsWithoutFieldEmptyInBoard([_|Fields], [0|PrePossibleActions], [0|PossibleActions]) :-
        actionsWithoutFieldEmptyInBoard(Fields, PrePossibleActions, PossibleActions),
        !.
    %.....

    actionsWithoutFieldEmptyInBoard([0|Fields], [_|PrePossibleActions], [0|PossibleActions]) :-
        actionsWithoutFieldEmptyInBoard(Fields, PrePossibleActions, PossibleActions),
        !.
    %.....

    actionsWithoutFieldEmptyInBoard([_|Fields], [_|PrePossibleActions], [1|PossibleActions]) :-
        actionsWithoutFieldEmptyInBoard(Fields, PrePossibleActions, PossibleActions).
    %.....

    actionsWithoutFieldEmptyInBoard([], [], []).

%-------
%actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions, &[PossibleActions])
%Actions qui nourissent l'adversaires
%-------
    actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions,PrePossibleActions) :-
        getEnemyBoard(GameState, EnemyBoard), enemyBoardIsNotEmpty(EnemyBoard),
        !.
    %.....

    actionsWithoutEnemyBoardEmpty(GameState, PrePossibleActions, PossibleActions) :-
        getPlayerBoard(GameState, PlayerBoard),
        actionsFeedEnemy(PlayerBoard, PrePossibleActions, PossibleActions),
        notZeroList(PossibleActions).

%-------
%actionsFeedEnemy(PlayerBoard, [PrePossibleActions], &[PossibleActions])
%-------
    actionsFeedEnemy(PlayerBoard, PrePossibleActions, PossibleActions) :-
        actionsFeedEnemy(PlayerBoard, PrePossibleActions, 1, PossibleActions).

%-------
%actionsFeedEnemy(PlayerBoard, [PrePossibleActions], FieldIndex, &[PossibleActions])
%-------
    actionsFeedEnemy([_|Fields], [0|PrePossibleActions], FieldIndex, [0|PossibleActions] ) :-
        NewFieldIndex is FieldIndex + 1,
        actionsFeedEnemy(Fields, PrePossibleActions, NewFieldIndex, PossibleActions).
    %.....

    actionsFeedEnemy([Field|Fields], [1|PrePossibleActions], FieldIndex, [PossibleAction|PossibleActions]) :-
        nbFields(NbFields),
        LastField is Field+FieldIndex,
        heaviside(LastField, NbFields+1, PossibleAction),
        NewFieldIndex is FieldIndex + 1,
        actionsFeedEnemy(Fields, PrePossibleActions, NewFieldIndex, PossibleActions).
    %.....

    actionsFeedEnemy([], [], _, []).


%******************%
%*	  DealSeeds	  *%
%******************%

%-------
%dealSeeds([Boards], PlayerTurn, ChoosedAction, &[NewBoards], &LastField)
%Distribution des graines
%-------
	dealSeeds([Board1,Board2], 0, ChoosedAction, [NewBoard1,NewBoard2], LastField) :-
        nbFields(NbFields),
		computeFirstIndex(ChoosedAction, FirstIndex),
		dealBoardActive(Board1, FirstIndex, TotalDealt, NewBoard1),
		FirstIndex2 is (FirstIndex+NbFields) mod (2*NbFields),
		dealBoardPassive(Board2, FirstIndex2, TotalDealt, NewBoard2),
		computeLastField(0, ChoosedAction, TotalDealt, LastField),
        !.
    %.....

	dealSeeds([Board1,Board2], 1, ChoosedAction, [NewBoard1,NewBoard2], LastField) :-
        nbFields(NbFields),
		computeFirstIndex(ChoosedAction, FirstIndex),
		dealBoardActive(Board2, FirstIndex, TotalDealt, NewBoard2),
		FirstIndex2 is (FirstIndex+NbFields) mod (2*NbFields),
		dealBoardPassive(Board1, FirstIndex2, TotalDealt, NewBoard1),
		computeLastField(1, ChoosedAction, TotalDealt, LastField), !.

%-------
%computeFirstIndex(ChoosedAction, &FirstIndex)
%-------
	computeFirstIndex(ChoosedAction, FirstIndex) :-
        nbFields(NbFields),
        FirstIndex is (2*NbFields+1 - ChoosedAction) mod (2*NbFields).

%-------
%dealBoardActive(Board, FirstIndex, &TotalDealt, &NewBoard)
%-------
	dealBoardActive([], _, _, []).
    %.....

	dealBoardActive([T|Q], 0, T, [0 | SubBoard]) :-
        dealBoardActive(Q, 1, T, SubBoard).
    %.....

	dealBoardActive([T|Q], Index, TotalDealt, [C | SubBoard]) :-
        nbFields(NbFields),
        NextIndex is (Index+1) mod (2*NbFields),
        dealBoardActive(Q, NextIndex, TotalDealt, SubBoard),
        C is T + ((TotalDealt-Index) div (2*NbFields-1)) + 1.

%-------
%dealBoardPassive(Board, FirstIndex, TotalDealt, &NewBoard)
%-------
	dealBoardPassive([], _, _, []).
    %.....

	dealBoardPassive(Board, Index, TotalDealt, NewBoard) :-
        nbFields(NbFields),
        AddedSeeds is ((TotalDealt-Index) div (2*NbFields-1)) + 1,
        subDealBoardPassive(Board, Index, TotalDealt, NewBoard, AddedSeeds).
    %.....


	subDealBoardPassive(SubB, _, _, SubB, AddedSeeds) :-
        AddedSeeds =:= 0,
        !.
    %.....

	subDealBoardPassive([T|Q], Index, TotalDealt, [C|SubBoard], AddedSeeds)	:-
        nbFields(NbFields),
        C is T+AddedSeeds,
        NextIndex is (Index+1) mod (2*NbFields),
        dealBoardPassive(Q, NextIndex, TotalDealt, SubBoard).
	
%-------
%computeLastField(PlayerTurn, ChoosedAction, TotalDealt, &LastField)
%-------
	computeLastField(PlayerTurn, ChoosedAction, TotalDealt, LastField) :-
        nbFields(NbFields),
        LastField is ((PlayerTurn*NbFields + ChoosedAction + TotalDealt + (TotalDealt div (2*NbFields)) - 1) mod (2*NbFields)) + 1.

%******************%
%*	   Harvest	  *%
%******************%

%-------
%harvestSeeds(GameState, LastField, &NewGameState)
%Recolte des graines
%-------
	harvestSeeds([Scores, Boards, PlayerTurn], LastField, [NewScores, NewBoards, PlayerTurn]) :-
        fieldIndexToPlayerIndex(LastField, EnemyPlayer),
        PlayerTurn \= EnemyPlayer,
        getEnemyBoard([_, Boards, PlayerTurn], EnemyBoard),
        getPlayerScore([Scores, _, PlayerTurn], Score),
        nbFields(NbFields),
        RelativeLastField is ((LastField-1) mod NbFields) + 1,
        harvestBoard(EnemyBoard, RelativeLastField, NewEnemyBoard, NewScore),
        enemyBoardIsNotEmpty(NewEnemyBoard), PlayerIndex is PlayerTurn +1,
        NewPlayerScore is Score+NewScore,
        replaceElementInListAtIndexWithElement(Scores, PlayerIndex, NewPlayerScore , NewScores),
        EnemyPlayerIndex is EnemyPlayer +1,
        replaceElementInListAtIndexWithElement(Boards, EnemyPlayerIndex, NewEnemyBoard, NewBoards), !.
	%.....
	
	harvestSeeds(GameState,_, GameState).


%-------
%enemyBoardIsNotEmpty(Board)
%Vérification que le plateau n'est pas vide
%-------
	enemyBoardIsNotEmpty(Board) :-
        notZeroList(Board).

%-------
%harvestBoard(Board, LastField, &NewBoard, &ScoreEarned)
%-------
	harvestBoard(Board, LastField, NewBoard, ScoreEarned) :-
        harvestBoard(Board, LastField, NewBoard, ScoreEarned, _).

%-------
%harvestBoard(Board, LastField, &NewBoard, &ScoreEarned, &Continue)
%-------

    harvestBoard([Field|Board], 1, [NewField|Board], ScoreEarned, Continue) :-
        harvestFieldIfPossible(Field, yes, NewField, ScoreEarned, Continue),
        !.
    %.....

	harvestBoard([Field|Board], LastField, [NewField|NewBoard], ScoreEarned, NewContinue) :-
        OffsetLastField is LastField-1,
        harvestBoard(Board, OffsetLastField, NewBoard, NewScoreEarned, Continue),
        harvestFieldIfPossible(Field, Continue, NewField, ActualScore, NewContinue),
        ScoreEarned is ActualScore+NewScoreEarned,
        !.


%-------
%harvestFieldIfPossible(Field, CanHarvest, &NewField, &ScoreEarned, &Continue)
%-------
	harvestFieldIfPossible(Field, yes, 0, ScoreEarned, yes) :-
        harvestField(Field, ScoreEarned).
	%.....
	
	harvestFieldIfPossible(Field, yes, Field, 0, no).
	%.....
	
	harvestFieldIfPossible(Field, no, Field, 0, no). 

%-------
%harvestField(Field, &ScoreEarned)
%-------
	harvestField(Field, Field) :-
        Field >= 2,
        Field =< 3,
        !.


%******************%
%*	   Empty	  *%
%******************%

%-------
%emptyBoards(GameState, &NewGameState)
%-------
	emptyBoards([[], [], _], [[], [], _]) :-
        !.
    %.....

	emptyBoards([[Score|Scores], [Board|Boards], _], [ [NewScore|NewScores], [NewBoard|NewBoards], _]) :-
        emptyBoards([Scores,Boards,_], [NewScores, NewBoards, _]),
        emptyBoard(Board, NewBoard, ScoreEarned),
        NewScore is Score+ScoreEarned.

%-------
%emptyBoard(Board, &NewBoard, &ScoreEarned)
%-------
	emptyBoard([], [], 0) :-
        !.
    %.....

	emptyBoard([Field|Fields], [0|NewFields], ScoreEarned) :-
        emptyBoard(Fields, NewFields, NewScoreEarned),
        ScoreEarned is NewScoreEarned + Field.
	
	