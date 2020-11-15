:- module(utils,
[
  % PREDICADOS GENERALES
  reemplazar/4, %
  %
  colapsar/2, % +Lista, -Resultado
  % Suma los valores de Lista en Resultado

  % PREDICADOS PARA JUEGO
  convertir_a_indice/2, % +Casa, -Indice
  % Dado un identificador de casa, lo convierte en un índice del 0 al 11
  separar_tablero/3, % +Tablero, -Casas1, -Casas2
  % Dada una lista Tablero, separa los primeros 6 elementos en Casas1 y los últimos 6 en Casas2
  es_humano/3, % +Jugador1, +Jugador2, +Turno
  % Determina si el jugador del cual es el turno es humano

  % PREDICADOS PARA ARCHIVOS
  cargar/2,  % +Estado
  % Carga el estado guardado en saves/estado.txt sobreescribiendo la partida actual
  guardar/2 % +Estado, +Visual
  % Guarda el estado actual de la partida en saves/estado.txt
]).

:- use_module(graficos).

% --------------------------
% Predicados generales
% --------------------------

reemplazar(0, E, [_|T], [E|T]).
reemplazar(P, E, [H|T], [H|R]) :-
    P > 0, NP is P-1, reemplazar(NP, E, T, R).

colapsar([H],H).
colapsar([H1,H2|T],Suma) :-
    colapsar([H1+H2|T],Suma).

% --------------------------
% Predicados para juego
% --------------------------

% array([A1, B1, C1, D1, E1, F1, A2, B2, C2, D2, E2, F2]).
convertir_a_indice(Casa, Indice) :- 
    nth0(Indice, [a1, b1, c1, d1, e1, f1, a2, b2, c2, d2, e2, f2], Casa).

separar_tablero([C1,C2,C3,C4,C5,C6|T], Casas1, Casas2) :-
    Casas1 = [C1,C2,C3,C4,C5,C6],
    Casas2 = T.

es_humano(humano, _Jugador2, jugador1).
es_humano(_Jugador1, humano, jugador2).

% --------------------------
% Predicados para archivos
% --------------------------

% Carga Estado = estado(Jugador1,Jugador2,Tablero,Score1,Score2,Turno) desde saves/estado.txt
cargar(Estado, Visual):-
    gr_pregunta(Visual, 'Nombre del archivo a guardar?', Nombre),
    string_concat(Nombre, '.j', Nombre_Archivo),
    string_concat('saves/', Nombre_Archivo, Path_Archivo),
    open(Path_Archivo,read,In),
    read_line_to_codes(In,Estado),
    close(In).

% Guarda Estado = estado(Jugador1,Jugador2,Tablero,Score1,Score2,Turno) en saves/estado.txt
guardar(Estado, Visual):-
    gr_pregunta(Visual, 'Nombre del archivo a guardar?', Nombre),
    string_concat(Nombre, '.j', Nombre_Archivo),
    string_concat('saves/', Nombre_Archivo, Path_Archivo),
    open(Path_Archivo,write,Out),
    write(Out,Estado),
    close(Out).
