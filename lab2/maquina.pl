:- module(maquina,
[
  iniciar_maquina/6  % +Profundidad, +Turno, +Tablero, +Score1, +Score2, -CasaElegida
  % Determina si el jugador del cual es el turno es humano
]).

:- use_module(core).
:- use_module(utils).

iniciar_maquina(X, Turno, Tablero, Score1, Score2, CasaElegida) :-
  minimax(Turno, ).

minimax(jugador1, Tablero, Score1, Score2, CasaElegida) :-
    separar_tablero(Tablero, Casas1, _),
    casas_viables(jugador1, Casas1, CasasViables),
    mejor_valor(jugador1, Tablero, Score1, Score2, CasasViables, CasaElegida, Valor), !
    ;
    evaluar(Score1, Score2, Valor).


mejor_valor(Jugador, Tablero, Score1, Score2, [CasaElegida], CasaElegida, Valor) :-
    
    minimax(Jugador, CasaElegida, _, Valor), !.
mejor_valor(Jugador, [Casa1 | CasasViables], Casa, Valor) :-
    minimax(Jugador, Casa1, _, Valor1),
    mejor_valor(Jugador, CasasViables, Casa2, Valor2),
    mejor_entre(Jugador, Casa1, Valor1, Casa2, Valor2, Casa, Valor).

mejor_entre(jugador1, Casa1, Valor1, _, Valor2, Casa1, Valor1) :-  
    Valor1 > Valor2, !.
mejor_entre(jugador2, Casa1, Valor1, _, Valor2, Casa1, Valor1) :- 
    Valor1 < Valor2, !.
mejor_entre(_, _, _, Casa2, Valor2, Casa2, Valor2).  

evaluar(Score1, Score2, Valor) :-
    Score1 > Score2
    ->
    Valor is Score1 - Score2
    ;
    Valor is Score2 - Score1.

terminal_node(Score1, Score2) :-
  Score1 > 24.
terminal_node(Score1, Score2) :-
  Score2 > 24.
terminal_node(Score1, Score2) :-
  (Score1 is 24, Score2 is 24).