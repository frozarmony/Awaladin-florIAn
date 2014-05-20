%Structure de données
PlayerState : 
[
	[PlayerType,...],
	[PlayerType,...]
]

PlayerType :
kHuman, kAssistedHuman, kComputer

GameState :
[
	[Score1,Score2],
	[Board1,Board2],
	PlayerTurn, 
]

Score :
Integer

Board :
[
	[Field1, Field2, ..., FieldN]
]

PlayerTurn :
Integer between 0 & J-1

Action : 
Integer between 0 & N-1

%Programme
awale. %Lance le jeu
	init(GameState,PlayerState). %Initialise les variables
	gameLoop([GameStateI],PlayerState,&[GameStates]). %Boucle principale du jeu
		gameTurn([GameStates], PlayerState, &NewGameState) %Un tour de jeu
			displayGameState(GameState)
			getPossibleActions(GameState, &PossibleActions)
			
				cyclicGame(Action)
				actionIsImpossible(Action)
					cantFeedOtherPlayer(Action)
					
						...
				
			//displayPossibleActions(PossibleActions)
		
			
			chooseAction(GameState, PlayerState, PossibleActions, &ChoosedAction)
				getBestAction() %Ordinateur
				askAction() %Humain
					getAdvisedAction(GameState, PlayerState, PossibleActions, &AdvisedAction)
					
					
			
			doAction(GameState, ChoosedAction, &NewGameState)
				dealSeeds([Boards], ChoosedAction, &[NewBoards], &LastField)						% Done
					compute(ChoosedAction, &FirstIndex)												% Done
					dealBoard(Board, FirstIndex, &TotalDealt, &NewBoard)							% Done
				harvestSeeds(GameState, LastField, &NewGameState)
					harvestFields(EnemyBoard, LastField, &NewEnemyBoard, &EarnedScore)				
						
						harvestField(EnemyBoard, &EarnedScore)
						
					.....
					-> harvestSeeds(GameState, LastField, GameState)
			.....
			emptyBoard(GameState, &NewGameState)
			
			
		endOfGame(GameState) %Vérifie si le jeu est fini
		
	displayEndOfGame(GameState, PlayerState).
	
	
	
%Questions importantes
- Qu'est-ce qu'un jeu cyclique ?
- Quelle structure de donnée pour l'IA ? Où la mettre ?
