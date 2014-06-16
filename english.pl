%***************%
%*	 ENGLISH   *%
%***************%

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