%
es_afd(af(Q,S,F)) :- estados(Q), simbolos(S), finales(F,Q).

%
es_afnd(af(Q,S,F)) :- estados(Q), simbolos(S), finales(F,Q).

%
es_afnd_eps(af(Q,S,F)) :- estados(Q), simbolos(S), finales(F,Q).

%
% reconoce(af,w)

% Metodos Auxiliares

% estados(Q)
% Comprueba si la lista de estados Q contiene naturales sin repetir e incluye al 0
estados([0]).
estados([0|T]) :- no_pertenece(0,T), estados_sin_cero(T).
estados([H|T]) :- number(H), H > 0, no_pertenece(H,T), estados(T).
estados_sin_cero([H]) :- number(H), H > 0.
estados_sin_cero([H|T]) :- number(H), H > 0, no_pertenece(H,T), estados_sin_cero(T).

% simbolos(S)
simbolos([H|[]]).
simbolos([H|T]) :- no_pertenece(H,T), simbolos(T).

% finales(F, Q)
% Comprueba si la lista de estados F se encuentra contenida en la lista de estados Q
finales(F, Q) :- contenida(F, Q).

%
% delta_afd().
% delta_afnd().
% delta_afnd_eps().

% Metodos Auxiliares (para listas)

% pertence(?X, ?L)
% El elemento X pertence a la lista L
pertenece(X,[X|_]).
pertenece(X,[_|T]):- pertenece(X,T).

% no_pertence(+X, +L)
% El elemento X no pertence a la lista L
no_pertenece(_,[]).
no_pertenece(X,[Y|T]):-X\=Y,no_pertenece(X,T).

% contenida(+L1, +L2)
% Todos los elementos de L1 pertencen a L2
contenida([], [_X|_Y]).
contenida([X|T], W) :- pertenece(X, W), contenida(T, W).