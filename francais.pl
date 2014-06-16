%***************%
%*	 ENGLISH   *%
%***************%

%-------
%writeWrongGameState
%-------
    writeWrongGameState :-
    write('Mauvais GameState').

%-------
%writeEmptyBoard
%-------
    writeEmptyBoard :-
        write('Chacun récupère ses graines !\n').

%-------
%writeWinner(Player)
%-------
    writeWinner(0) :-
        write('J1 gagne ! ').
    %.....

    writeWinner(1) :-
        write('J2 gagne ! ').

%-------
%writePlayersTurn(PlayerTurn)
%-------
    writePlayersTurn(0) :-
        write('Tour de J1.\n').
    %.....

    writePlayersTurn(1) :-
        write('Tour de J2.\n').
%-------
%writeScore
%-------
    writeScore :-
        write('Score ').

%-------
%writeDraw
%-------
    writeDraw :-
        write('Egalité !').

%-------
%writePossibleActions
%-------
    writePossibleActions :-
        write('Actions possibles : ').

%-------
%writeActions
%-------
    writeActions :-
        write('Action ? :').

%-------
%writeSelectionMenu(Player)
%-------
    writeSelectionMenu(0) :-
        write('Menu J1 :\n').
    %.....

    writeSelectionMenu(1) :-
        write('Menu J2 :\n').

%-------
%writeWrongInput
%-------
    writeWrongInput :-
        write('Loupé ! Essaye encore !\n').

%-------
%writePlayerType(PlayerType)
%-------
    writePlayerType(kHuman) :-
        write('Humain').
    %.....

    writePlayerType(kAssistedHuman) :-
        write('Humain assisté').
    %.....

    writePlayerType(kComputer) :-
        write('Ordinateur').

%-------
%writeChoose
%-------
    writeChoose :-
        write('Choix : ').

%-------
%writeChoosedAction
%-------
    writeChoosedAction :-
        write('Action choisie : ').

%-------
%writeAdvisedAction
%-------
    writeAdvisedAction :-
        write('Action conseillée : ').