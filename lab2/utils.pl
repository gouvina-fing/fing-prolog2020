:- module(utils,
[
  movimiento/4, % +Jugador, +Tablero, -Nuevo_Tablero, +Score_Jugador, -Score_Final_Jugador, +Casillero_final
  % Devuelve en Score_Jugador el score de recoger semillas en el casillero final
  cargar/1,  % +Estado
  % Carga el estado guardado en saves/estado.txt sobreescribiendo la partida actual
  guardar/1 % +Estado
  % Guarda el estado actual de la partida en saves/estado.txt
]).

% array([A1, B1, C1, D1, E1, F1, A2, B2, C2, D2, E2, F2]).
convertir_a_indice(Casa, Indice) :- 
  nth0(Indice, [a1, b1, c1, d1, e1, f1, a2, b2, c2, d2, e2, f2], Casa).

% 
reemplazar(0, E, [_|T], [E|T]).
reemplazar(P, E, [H|T], [H|R]) :-
    P > 0, NP is P-1, reemplazar(NP, E, T, R).

% repartir_semillas(Final, Tablero, Tablero, 0, Final).

repartir_semillas(Final, Tablero, NuevoTablero, 0, Final) :-
  nth0(Final, Tablero, Semillas),
  MasUnaSemilla is Semillas + 1,
  reemplazar(Final, MasUnaSemilla, Tablero, NuevoTablero).

repartir_semillas(Indice, Tablero, NuevoTablero, Restantes, Final) :-
  NuevoIndice is Indice + 1,
  ModuleIndice is NuevoIndice mod 12,
  NuevoRestantes is Restantes - 1,
  nth0(ModuleIndice, Tablero, Semillas),
  MasUnaSemilla is Semillas + 1,
  reemplazar(ModuleIndice, MasUnaSemilla, Tablero, TableroActual),
  repartir_semillas(ModuleIndice, TableroActual, NuevoTablero, NuevoRestantes, Final).
  
movimiento(Casa, Tablero, NuevoTablero, Final) :-
  convertir_a_indice(Casa, Indice),
  nth0(Indice, Tablero, Semillas),
  Semillas > 0,
  writeln(Semillas),
  reemplazar(Indice, 0, Tablero, TableroActual),
  repartir_semillas(Indice, TableroActual, NuevoTablero, Semillas, Final).

% Carga Estado = estado(Jugador1,Jugador2,Tablero,Score1,Score2,Turno) desde saves/estado.txt
cargar(Estado):-
    open('saves/estado.txt',read,In),
    read_line_to_codes(In,Estado),
    close(In).

% Guarda Estado = estado(Jugador1,Jugador2,Tablero,Score1,Score2,Turno) en saves/estado.txt
guardar(Estado):-
    open('saves/estado.txt',write,Out),
    write(Out,Estado),
    close(Out).
