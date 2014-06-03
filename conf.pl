% Conf File

% Flush Old Conf
:- rectractAll(initGameState(_)).

% Set Conf
:- asserta(initGameState([[0,0],[[4,4,4,4,4,4],[4,4,4,4,4,4]],0])).

:- asserta(totalSeeds(48)).
:- asserta(nbFields(6)).

% Tool to deduce conf from initGameState
getConf([_, [fields1, fields2], _]) :-
		
