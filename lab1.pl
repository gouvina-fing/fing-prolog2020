% largo(+L, ?N)
% Retorna el largo de la lista L
largo([],0).
largo([_|T],L1):-largo(T,L2),L1 is L2+1.

% pertence(?X, ?L)
% El elemento X pertence a la lista L
pertenece(X,[X|_]).
pertenece(X,[_|T]):- pertenece(X,T).

% no_pertence(+X, +L)
% El elemento X no pertence a la lista L
no_pertenece(_,[]).
no_pertenece(X,[Y]):-X\=Y.
no_pertenece(X,[Y|T]):-X\=Y,no_pertenece(X,T).

% elegir(?X, ?L1, ?L2)
% La lista L2 resulta de eliminiar un elemento de la lista L1
elegir(X, [X|T], T).
elegir(X, [Z|T], [Z|O]) :- elegir(X, T, O).

% contenida(+L1, +L2)
% Todos los elementos de L1 pertencen a L2
contenida([], [_X|_Y]).
contenida([X|T], W) :- pertenece(X, W), contenida(T, W).

% sublista(+L, ?Sub)
% Sub contiene un subconjunto de elementos contiguos de L en el mismo orden que aparecen en L
sublista(Z, X) :- prefijo(Z, X).
sublista([_|T], X) :- prefijo(T, X).

% permutacion(+L1, ?L2)
% La lista L2 es una permutacion de la lista L1, el cual no tiene repetidos
permutacion([], []).
permutacion([X|Y], Z) :- permutacion(Y, W), elegir(X,Z,W).

% arreglo(+L1, +N, ?L2)
% La lista L2 es un arreglo de N elementos de la lista L1
arreglo(L, N, X) :- combinacion(L, N, W), permutacion(W, X).

% arreglo_con_repeticion(+L1, +N, ?L2)
% La lista L2 es un arreglo con repeticion de N elementos de la lista L1
arreglo_con_repeticion(_, 0, []).
arreglo_con_repeticion([H|T], N, [H|Tp]) :- N>0, Np is N-1, arreglo_con_repeticion([H|T], Np, Tp).
arreglo_con_repeticion([_|T], N, X) :- N>0, arreglo_con_repeticion(T, N, X).

% combinacion(+L1, +N, -L2)
% La lista L2 es una combinacion de N elementos de la lista L1.
combinacion([], _, []).
combinacion([H|W], N, [H|T]) :- Z is N-1, combinacion(W, Z, T), largo(T, Z).
combinacion([_|T], N, X) :- combinacion(T, N, X), largo(X, N).

% Metodos Auxiliares

% prefijo(+L1, ?L2)
% L2 es prefijo de L1
prefijo(_,[]).
prefijo([H|T],[Hp|Tp]):-H=Hp,prefijo(T,Tp).