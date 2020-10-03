% TT = Tamaño del tablero

no_pertenece(_,[]).
no_pertenece(X,[Y|T]) :- not(X == Y), no_pertenece(X,T).

sin_repetidos([]).
sin_repetidos([H|T]) :- no_pertenece(H,T), sin_repetidos(T).

% quitar_n_primeros(0, Lista, Lista).
% quitar_n_primeros(N, [H|T], T) :-
%   X is N - 1.
% quitar_n_primeros(X, )

casilla_valida(c(X,Y),TT) :-
  TT >= X,
  TT >= Y,
  X  >  0,
  Y  >  0.

movimiento_valido(c(X1,Y1), c(X2,Y2), TT) :- 
  (
     X2 is  X1 - 2,
     Y2 is  Y1 - 1
    ;
     X2 is  X1 - 2,
     Y2 is  Y1 + 1
    ;
     X2 is  X1 + 2,
     Y2 is  Y1 - 1
    ;
     X2 is  X1 + 2,
     Y2 is  Y1 + 1
    ;
     X2 is  X1 - 1,
     Y2 is  Y1 - 2
    ;
     X2 is  X1 - 1,
     Y2 is  Y1 + 2
    ;
     X2 is  X1 + 1,
     Y2 is  Y1 - 2
    ;
     X2 is  X1 + 1,
     Y2 is  Y1 + 2
  ),
  (
    casilla_valida(c(X1,Y1), TT),
    casilla_valida(c(X2,Y2), TT)
  ).

recorrido_caballo_auxiliar(_1, 0, _3, [_4|Resto], _5):-Resto=[].

recorrido_caballo_auxiliar(TT, 1, Casillas_prohibidas, [Casilla_actual | c(X,Y)], Recorrido) :-
  movimiento_valido(Casilla_actual, c(X,Y), TT),
  no_pertenece(c(X,Y), Casillas_prohibidas),
  sin_repetidos(Recorrido).

recorrido_caballo_auxiliar(TT, Numero_movimientos, Casillas_prohibidas, [Casilla_actual | [c(X,Y) | Resto]], Recorrido) :-
  Numero_movimientos > 0,
  movimiento_valido(Casilla_actual, c(X,Y), TT),
  no_pertenece(c(X,Y), Casillas_prohibidas),
  sin_repetidos(Recorrido),
  Restantes is Numero_movimientos - 1,
  recorrido_caballo_auxiliar(TT, Restantes, Casillas_prohibidas, [c(X,Y) | Resto], Recorrido).

recorrido_caballo(TT, Numero_movimientos, Casillas_prohibidas, Recorrido) :-
  recorrido_caballo_auxiliar(TT, Numero_movimientos, Casillas_prohibidas, [c(1,2) | Resto], [c(1,2) | Resto]),
  Recorrido = [c(1,2) | Resto].
  
