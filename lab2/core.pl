:- module(core,
[
 recoger_semillas/6, % +Jugador, +Tablero, -Nuevo_Tablero, +Score_Jugador, -Score_Final_Jugador, +Casillero_final
 % Devuelve en Score_Jugador el score de recoger semillas en el casillero final
 separar_tablero/3 % +Tablero, -Casas1, -Casas2
 % Dada una lista Tablero, separa los primeros 6 elementos en Casas1 y los últimos 6 en Casas2
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
  %reemplazar()
  recoger_semillas(Jugador, Tablero, New_Score, Score_Final_Jugador, New_Casillero_final).


% A
separar_tablero([C1,C2,C3,C4,C5,C6|T], Casas1, Casas2) :-
  Casas1 = [C1,C2,C3,C4,C5,C6],
  Casas2 = T.

