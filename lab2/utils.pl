:- module(utils,
[
  % PREDICADOS GENERALES
  reemplazar/4, %
  %

  % PREDICADOS PARA JUEGO
  convertir_a_indice/2, %
  %
  separar_tablero/3, % +Tablero, -Casas1, -Casas2
  % Dada una lista Tablero, separa los primeros 6 elementos en Casas1 y los últimos 6 en Casas2

  % PREDICADOS PARA ARCHIVOS
  cargar/1,  % +Estado
  % Carga el estado guardado en saves/estado.txt sobreescribiendo la partida actual
  guardar/1 % +Estado
  % Guarda el estado actual de la partida en saves/estado.txt
]).

% PREDICADOS GENERALES

% 
reemplazar(0, E, [_|T], [E|T]).
reemplazar(P, E, [H|T], [H|R]) :-
    P > 0, NP is P-1, reemplazar(NP, E, T, R).

% PREDICADOS PARA JUEGO

% array([A1, B1, C1, D1, E1, F1, A2, B2, C2, D2, E2, F2]).
convertir_a_indice(Casa, Indice) :- 
  nth0(Indice, [a1, b1, c1, d1, e1, f1, a2, b2, c2, d2, e2, f2], Casa).

% A
separar_tablero([C1,C2,C3,C4,C5,C6|T], Casas1, Casas2) :-
  Casas1 = [C1,C2,C3,C4,C5,C6],
  Casas2 = T.

% PREDICADOS PARA ARCHIVOS

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
