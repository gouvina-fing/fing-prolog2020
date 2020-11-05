:- module(core,
[
 recoger_semillas/4 % +Jugador, +Tablero, -Nuevo_Tablero, +Score_Jugador, -Score_Final_Jugador, +Casillero_final
 % Devuelve en Score_Jugador el score de recoger semillas en el casillero final
]).

recoger_semillas(Jugador, Tablero, Nuevo_Tablero, Score_Jugador, Score_Jugador, Casillero_final) :-
  Jugador = jugador1,
  Casillero_final < 6.
recoger_semillas(Jugador, Tablero, Nuevo_Tablero, Score_Jugador, Score_Jugador, Casillero_final) :-
  Jugador = jugador2,
  Casillero_final > 6.

recoger_semillas(Jugador, Tablero, Nuevo_Tablero, Score_Jugador, Score_Jugador, Casillero_final) :-
  nth0(Casillero_final, Tablero, Numero_semillas),
  Numero_semillas < 2.

recoger_semillas(Jugador, Tablero, Nuevo_Tablero, Score_Jugador, Score_Jugador, Casillero_final) :-
  nth0(Casillero_final, Tablero, Numero_semillas),
  Numero_semillas > 3.

recoger_semillas(Jugador, Tablero, Nuevo_Tablero, Score_Jugador, Score_Final_Jugador, Casillero_final) :-
  nth0(Casillero_final, Tablero, Numero_semillas),
  New_Score is Score_Jugador + Numero_semillas,
  New_Casillero_final is Casillero_final - 1,
  reemplazar()
  recoger_semillas(Jugador, Tablero, New_Score, Score_Final_Jugador, New_Casillero_final).

reemplazar(0, E, [_|T], [E|T]).
reemplazar(P, E, [H|T], [H|R]) :-
    P > 0, NP is P-1, reemplazar(NP, E, T, R).

