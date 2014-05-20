% Awaladin-FloIAn

%-------
%init(&GameState,&PlayerState)
%-------
	init([[0,0],[[4,4,4,4,4,4],[4,4,4,4,4,4]],0], [[TypeJ1],[TypeJ2]]) :- choosePlayerType(TypeJ1,TypeJ2).
	
	choosePlayerType(TypeJ1,TypeJ2) 	:-
		repeat, write('J1 Selection Menu :\n'), choosePlayerType(TypeJ1), !,
		repeat, write('J2 Selection Menu :\n'), choosePlayerType(TypeJ2), !.
	choosePlayerType(TypeJ)				:-
		write('1. Human\n'),write('2. Assisted Human\n'),write('3. IA\n'), write('Choose : '),
		read(Number), interpretNumber(Number, TypeJ).
		
	interpretNumber(1, kHuman)			:- !.
	interpretNumber(2, kAssistedHuman)	:- !.
	interpretNumber(3, kComputer)		:- !.
	interpretNumber(_, _) :- write('Invalid input, try again!!\n'), fail.