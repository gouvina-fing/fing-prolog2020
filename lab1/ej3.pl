% Determina si af es un afd
es_afd(af(Q,S,F, Delta)) :- estados(Q), simbolos(S), finales(F,Q), delta_afd(Q, S, Delta).

% Determina si af es un afnd
es_afnd(af(Q,S,F, Delta)) :- estados(Q), simbolos(S), finales(F,Q), delta_afnd(Q, S, Delta).

% Determina si af es un afnd-epsilon
es_afnd_eps(af(Q,S,F, Delta)) :- estados(Q), simbolos(S), finales(F,Q), delta_afnd_eps(Q, S, Delta).

% Determina si la tira W es reconocida por el automata finito af
reconoce(af(Q,S,F,Delta), W) :- es_afd(af(Q,S,F,Delta)), reconoce_no_eps(af(Q,S,F,Delta), W).
reconoce(af(Q,S,F,Delta), W) :- es_afnd(af(Q,S,F,Delta)), reconoce_no_eps(af(Q,S,F,Delta), W).
reconoce(af(Q,S,F,Delta), W) :- es_afnd_eps(af(Q,S,F,Delta)), reconoce_eps(af(Q,S,F,Delta), W).

% Metodos Auxiliares (para reconocimiento)

% Caso afd y afnd: se usan 2 predicados
% reconoce_no_eps controla que el primer símbolo se consuma en el primer estado
reconoce_no_eps(af(Q,S,F,Delta), [H]) :- pertenece(d(0,Y,H), Delta), pertenece(Y,F).
reconoce_no_eps(af(Q,S,F,Delta), [H|T]) :- pertenece(d(0,Y,H), Delta), reconoce_no_eps_siguiente(af(Q,S,F,Delta), T, Y).
% reconoce_no_eps_siguiente es recursivo y va consumiendo de la tira. Tiene como parametro adicional el siguiente estado, para hacer avanzar tira y estado
reconoce_no_eps_siguiente(af(Q,S,F,Delta), [H], X) :- pertenece(d(X,Y,H), Delta), pertenece(Y,F).
reconoce_no_eps_siguiente(af(Q,S,F,Delta), [H|T], X) :- pertenece(d(X,Y,H), Delta), reconoce_no_eps_siguiente(af(Q,S,F,Delta), T, Y).

% Caso afnd_eps: se usan 3 predicados
% reconoce_eps controla que el primer símbolo se consuma en el primer estado, y agrega la posibilidad de transiciones epsilon antes de consumir el primer símbolo
reconoce_eps(af(Q,S,F,Delta), [H]) :- pertenece(d(0,Y,H), Delta), pertenece(Y,F). % Caso sin epsilon, 1 elemento
reconoce_eps(af(Q,S,F,Delta), [H]) :- pertenece(d(0,Y,epsilon), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [H], Y). % Caso epsilon al principio, 1 elemento
reconoce_eps(af(Q,S,F,Delta), [H|T]) :- pertenece(d(0,Y,H), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), T, Y). % Caso sin epsilon, 2+ elementos
reconoce_eps(af(Q,S,F,Delta), [H|T]) :- pertenece(d(0,Y,epsilon), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [H|T], Y). % Caso epsilon al principio, 2+ elementos
% reconoce_eps_siguiente es recursivo y va consumiendo de la tira. Tiene como parametro adicional el siguiente estado, para hacer avanzar tira y estado
% -- Tira completamente consumida. Si no se llega a estado final, se avanza a caso borde con reconcoe_eps_final
reconoce_eps_siguiente(af(Q,S,F,Delta), [], X) :- pertenece(d(X,Y,epsilon), Delta), pertenece(Y,F). % Caso 1 epsilon al final
reconoce_eps_siguiente(af(Q,S,F,Delta), [], X) :- pertenece(d(X,Y,epsilon), Delta), reconoce_eps_final(af(Q,S,F,Delta), Y). % Caso 1+ epsilon al final
% -- Tira con un solo simbolo por consumir
reconoce_eps_siguiente(af(Q,S,F,Delta), [H], X) :- pertenece(d(X,Y,H), Delta), pertenece(Y,F). % Caso sin epsilon final, 1 elemento (2 originales)
reconoce_eps_siguiente(af(Q,S,F,Delta), [H], X) :- pertenece(d(X,Y,H), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [], Y). % Caso sin epsilon intermedio, 2+ elementos (3+ originales)
reconoce_eps_siguiente(af(Q,S,F,Delta), [H], X) :- pertenece(d(X,Y,epsilon), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [H], Y). % Caso con epsilon intermedio, 2+ elementos (3+ originales)
% -- Tira con más de un simbolo por consumir
reconoce_eps_siguiente(af(Q,S,F,Delta), [H|T], X) :- pertenece(d(X,Y,H), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), T, Y).
reconoce_eps_siguiente(af(Q,S,F,Delta), [H|T], X) :- pertenece(d(X,Y,epsilon), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [H|T], Y).
% reconoce_eps_final controla si se consumió toda la tira pero hay un camino de largo finito hasta un estado final a través de transiciones epsilon
reconoce_eps_final(af(Q,S,F,[d(X,Y,epsilon)|T]), X) :- pertenece(Y,F). % Caso 1 epsilon al final
reconoce_eps_final(af(Q,S,F,[d(X,Y,epsilon)|T]), X) :- reconoce_eps_final(af(Q,S,F,T), Y). % Caso 2+ epsilon al final


% Metodos Auxiliares (para automatas)

% estados(+Q)
% Comprueba si la lista de estados Q contiene naturales sin repetir e incluye al 0
estados([0]).
estados([0|T]) :- no_pertenece(0,T), estados_sin_cero(T).
estados([H|T]) :- integer(H), H > 0, no_pertenece(H,T), estados(T).
estados_sin_cero([H]) :- integer(H), H > 0.
estados_sin_cero([H|T]) :- integer(H), H > 0, no_pertenece(H,T), estados_sin_cero(T).

% simbolos(+S)
% Comprueba si la lista de simbolos S contiene elementos atómicos sin repetir
simbolos([_|[]]).
simbolos([H|T]) :- no_pertenece(H,T), simbolos(T).

% finales(+F, +Q)
% Comprueba si la lista de estados F se encuentra contenida en la lista de estados Q
finales(F, Q) :- contenida(F, Q).

% delta_afd(Q, S, Delta)
% Comprueba que la función delta contenga transiciones correctas para un afd
delta_afd(_Q, _S, []).
delta_afd(Q, S, [d(O, D, Sym)|T]) :-
  pertenece(O,Q),
  pertenece(D,Q),
  Sym \= epsilon,
  pertenece(Sym,S),
  es_transicion_determinista([d(O, D, Sym)|T]),
  delta_afd(Q, S, T).

es_transicion_determinista([_]).
es_transicion_determinista([d(O, _D, S)|T]) :-
  origenesSimbolos(T, OrigenesSimbolos),
  no_pertenece(os(O,S), OrigenesSimbolos).

origenesSimbolos([],[]).
origenesSimbolos([d(O, _D, S)|T], [os(O,S)|TO]) :- origenesSimbolos(T,TO).

% delta_afnd(Q, S, Delta)
% Comprueba que la función delta contenga transiciones correctas para un afnd
delta_afnd(_Q, _S, []).
delta_afnd(Q, S, [d(O, D, Sym)|T]) :- 
  pertenece(O,Q),
  pertenece(D,Q),
  Sym \= epsilon,
  pertenece(Sym,S),
  delta_afnd(Q, S, T).

% delta_afnd_eps(Q, S, Delta)
% Comprueba que la función delta contenga transiciones correctas para un afnd-epsilon
delta_afnd_eps(_Q, _S, []).
delta_afnd_eps(Q, S, [d(O, D, Sym)|T]) :-
  pertenece(O,Q),
  pertenece(D,Q),
  (pertenece(Sym,S); Sym = epsilon),
  delta_afnd_eps(Q, S, T).


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