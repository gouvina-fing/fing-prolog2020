:- module(utils, [movimiento/4]).

array([A1, B1, C1, D1, E1, F1, A2, B2, C2, D2, E2, F2]).
array([a1, b1, c1, d1, e1, f1, a2, b2, c2, d2, e2, f2]).


reemplazar(0, E, [_|T], [E|T]).
reemplazar(P, E, [H|T], [H|R]) :-
    P > 0, NP is P-1, reemplazar(NP, E, T, R).

sumar1derecha(Final, Tablero, NuevoTablero, 1, Final) :-
  nth0(Final, Tablero, Semillas),
  MasUnaSemilla is Semillas + 1,
  reemplazar(Final, MasUnaSemilla, Tablero, NuevoTablero).

sumar1derecha(Indice, Tablero, NuevoTablero, Restantes, Final) :-
  NuevoIndice is Indice + 1,
  ModuleIndice is NuevoIndice mod 12,
  NuevoRestantes is Restantes - 1,
  nth0(Indice, Tablero, Semillas),
  MasUnaSemilla is Semillas + 1,
  reemplazar(Indice, MasUnaSemilla, Tablero, TableroActual),
  sumar1derecha(ModuleIndice, TableroActual, NuevoTablero, NuevoRestantes, Final).
  
movimiento(Indice, Tablero, NuevoTablero, Final) :-
  nth0(Indice, Tablero, Semillas),
  reemplazar(Indice, 0, Tablero, TableroActual),
  NuevoIndice is Indice + 1,
  sumar1derecha(NuevoIndice, TableroActual, NuevoTablero, Semillas, Final).
