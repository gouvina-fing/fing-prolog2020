%
es_afd(af(Q,S,F, Delta)) :- estados(Q), simbolos(S), finales(F,Q), delta_afd(Q, Delta).

%
es_afnd(af(Q,S,F, Delta)) :- estados(Q), simbolos(S), finales(F,Q), delta_afnd(Q, Delta).

%
es_afnd_eps(af(Q,S,F, Delta)) :- estados(Q), simbolos(S), finales(F,Q), delta_afnd_eps(Q, Delta).

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
simbolos([_|[]]).
simbolos([H|T]) :- no_pertenece(H,T), simbolos(T).

% finales(F, Q)
% Comprueba si la lista de estados F se encuentra contenida en la lista de estados Q
finales(F, Q) :- contenida(F, Q).

%
delta_afd(_Q, []).
delta_afd(Q, [d(O, D, Sym)|T]) :-
  pertenece(O,Q),
  pertenece(D,Q),
  Sym \= epsilon,
  atom(Sym),
  es_transicion_determinista([d(O, D, Sym)|T]),
  delta_afd(Q, T).

es_transicion_determinista([_]).
es_transicion_determinista([d(O, _D, S)|T]) :-
  origenesSimbolos(T, OrigenesSimbolos),
  no_pertenece(os(O,S), OrigenesSimbolos).

origenesSimbolos([],[]).
origenesSimbolos([d(O, _D, S) | T], [os(O,S)|TO]) :- origenesSimbolos(T,TO).

delta_afnd(_Q, []).
delta_afnd(Q, [d(O, D, Sym)|T]) :- 
  pertenece(O,Q),
  pertenece(D,Q),
  Sym \= epsilon,
  atom(Sym),
  delta_afnd(Q, T).


delta_afnd_eps(_Q, []).
delta_afnd_eps(Q, [d(O, D, Sym)|T]) :-
  pertenece(O,Q),
  pertenece(D,Q),
  atom(Sym),
  delta_afnd_eps(Q, T).


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