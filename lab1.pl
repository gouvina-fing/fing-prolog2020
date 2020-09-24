largo([],0).
largo([_|T],L1):-largo(T,L2),L1 is L2+1.

pertenece(X,[X|_]).
pertenece(X,[_|T]):- pertenece(X,T).

no_pertenece(_,[]).
no_pertenece(X,[Y]):-X\=Y.
no_pertenece(X,[Y|T]):-X\=Y,no_pertenece(X,T).

elegir(X, [X|T], T).
elegir(X, [Z|T], [Z|O]) :- elegir(X, T, O).


contenida([], [_X|_Y]).
contenida([X|T], W) :- pertenece(X, W), contenida(T, W).

sublista(Z, X) :- prefijo(Z, X).
sublista([_|T], X) :- prefijo(T, X).

permutacion([], []).
permutacion([X|Y], Z) :- permutacion(Y, W), elegir(X,Z,W).

arreglo(L, N, X) :- combinacion(L, N, W), permutacion(W, X).

arreglo_con_repeticion(_, 0, []).
arreglo_con_repeticion([H|T], N, [H|Tp]) :- N>0, Np is N-1, arreglo_con_repeticion([H|T], Np, Tp).
arreglo_con_repeticion([_|T], N, X) :- N>0, arreglo_con_repeticion(T, N, X).

combinacion([], _, []).
combinacion([H|W], N, [H|T]) :- Z is N-1, combinacion(W, Z, T), largo(T, Z).
combinacion([_|T], N, X) :- combinacion(T, N, X), largo(X, N).

prefijo(_,[]).
prefijo([H|T],[Hp|Tp]):-H=Hp,prefijo(T,Tp).