% EJERCICIO 1
% --------------------------------------------------------------------

% largo(+L, ?N)
% Retorna el largo de la lista L
largo([],0).
largo([_|T],L1):- largo(T,L2),L1 is L2+1.

% pertenece(?X, ?L)
% El elemento X pertenece a la lista L
pertenece(X,[X|_]).
pertenece(X,[_|T]):- pertenece(X,T).

% no_pertenece(+X, +L)
% El elemento X no pertenece a la lista L
no_pertenece(_,[]).
no_pertenece(X,[Y|T]) :- X \= Y, no_pertenece(X,T).

% elegir(?X, ?L1, ?L2)
% La lista L2 resulta de eliminiar un elemento de la lista L1
elegir(X, [X|T], T).
elegir(X, [Z|T], [Z|O]) :- elegir(X, T, O).

% contenida(+L1, +L2)
% Todos los elementos de L1 pertenecen a L2
contenida([], [_X|_Y]).
contenida([X|T], W) :- pertenece(X, W), contenida(T, W).

% sublista(+L, ?Sub)
% Sub contiene un subcopenjunto de elementos contiguos de L en el mismo orden que aparecen en L
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
arreglo_con_repeticion(L, N, [H|T]) :- N>0, Np is N-1, pertenece(H, L), arreglo_con_repeticion(L, Np, T).

% combinacion(+L1, +N, -L2)
% La lista L2 es una combinacion de N elementos de la lista L1.
combinacion([], _, []).
combinacion([H|W], N, [H|T]) :- Z is N-1, combinacion(W, Z, T), largo(T, Z).
combinacion([_|T], N, X) :- combinacion(T, N, X), largo(X, N).

% Metodos Auxiliares - Ejercicio 1

% prefijo(+L1, ?L2)
% L2 es prefijo de L1
prefijo(_,[]).
prefijo([H|T],[Hp|Tp]):-H=Hp,prefijo(T,Tp).

% EJERCICIO 2 
% --------------------------------------------------------------------

% Invoca el predicado auxiliar con la casilla inicial y la incluye en la lista de prohibidas
recorrido_caballo(TT, N, Casillas_prohibidas, Recorrido) :- 
  recorrido_caballo_auxiliar(TT, N, c(1,2), [c(1,2)|Casillas_prohibidas], Recorrido).

% Si no ha más movimientos, la casilla restante debe ser la casilla actual.
recorrido_caballo_auxiliar(_TT, 0, Casilla_Final, _, [Casilla_Final]).

% La casilla actual es la primera en la lista de movimientos restantes.
recorrido_caballo_auxiliar(TT, N1, Casilla_actual, Casillas_prohibidas, [Casilla_actual | Resto]) :-
  N1 > 0, % Chequea que haya movimientos disponibles
  movimiento_valido(Casilla_actual, Casilla_siguiente, TT), % Genera los siguientes movimientos posibles
  no_pertenece(Casilla_siguiente, Casillas_prohibidas), % Chequea que la siguiente casilla no este dentro de las prohibidas
  Nuevas_Casillas_Prohibidas = [Casilla_siguiente | Casillas_prohibidas], % se incluye la siguiente casilla dentro de las prohibidas
  N is N1 - 1, % Resta 1 a los movimientos restantes
  recorrido_caballo_auxiliar(TT, N, Casilla_siguiente, Nuevas_Casillas_Prohibidas, Resto). %% Llamado recursivo
  

% Metodos Auxiliares - Ejercicio 2

% casilla_valida(+C, +TT)
% Verifica que, dado un tablero y una casilla, la última este dentro del tablero
casilla_valida(c(X,Y),TT) :-
  TT >= X,
  TT >= Y,
  X  >  0,
  Y  >  0.

% movimiento_valido(+CO, ?CD, +TT)
% Verifica que un movimiento con casilla de origen CO y casilla de origen CD sea válido para el tablero de tamaño TT
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


% EJERCICIO 3
% --------------------------------------------------------------------

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

% Metodos Auxiliares - Ejercicio 3

% Metodos Auxiliares (para reconocimiento)

% Caso afd y afnd: se usan 2 predicados
% reconoce_no_eps controla que el primer símbolo se consuma en el primer estado
reconoce_no_eps(af(_,_,F,Delta), [H]) :- pertenece(d(0,Y,H), Delta), pertenece(Y,F).
reconoce_no_eps(af(Q,S,F,Delta), [H|T]) :- pertenece(d(0,Y,H), Delta), reconoce_no_eps_siguiente(af(Q,S,F,Delta), T, Y).
% reconoce_no_eps_siguiente es recursivo y va consumiendo de la tira. Tiene como parametro adicional el siguiente estado, para hacer avanzar tira y estado
reconoce_no_eps_siguiente(af(_Q,_,F,Delta), [H], X) :- pertenece(d(X,Y,H), Delta), pertenece(Y,F).
reconoce_no_eps_siguiente(af(Q,S,F,Delta), [H|T], X) :- pertenece(d(X,Y,H), Delta), reconoce_no_eps_siguiente(af(Q,S,F,Delta), T, Y).

% Caso afnd_eps: se usan 3 predicados
% reconoce_eps controla que el primer símbolo se consuma en el primer estado, y agrega la posibilidad de transiciones epsilon antes de consumir el primer símbolo
reconoce_eps(af(_,_,F,Delta), [H]) :- pertenece(d(0,Y,H), Delta), pertenece(Y,F). % Caso sin epsilon, 1 elemento
reconoce_eps(af(Q,S,F,Delta), [H]) :- pertenece(d(0,Y,epsilon), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [H], Y). % Caso epsilon al principio, 1 elemento
reconoce_eps(af(Q,S,F,Delta), [H|T]) :- pertenece(d(0,Y,H), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), T, Y). % Caso sin epsilon, 2+ elementos
reconoce_eps(af(Q,S,F,Delta), [H|T]) :- pertenece(d(0,Y,epsilon), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [H|T], Y). % Caso epsilon al principio, 2+ elementos
% reconoce_eps_siguiente es recursivo y va consumiendo de la tira. Tiene como parametro adicional el siguiente estado, para hacer avanzar tira y estado
% -- Tira completamente consumida. Si no se llega a estado final, se avanza a caso borde con reconcoe_eps_final
reconoce_eps_siguiente(af(_Q,_,F,Delta), [], X) :- pertenece(d(X,Y,epsilon), Delta), pertenece(Y,F). % Caso 1 epsilon al final
reconoce_eps_siguiente(af(Q,S,F,Delta), [], X) :- pertenece(d(X,Y,epsilon), Delta), reconoce_eps_final(af(Q,S,F,Delta), Y). % Caso 2+ epsilon al final
% -- Tira con un solo simbolo por consumir
reconoce_eps_siguiente(af(_,_,F,Delta), [H], X) :- pertenece(d(X,Y,H), Delta), pertenece(Y,F). % Caso sin epsilon final, 1 elemento (2 originales)
reconoce_eps_siguiente(af(Q,S,F,Delta), [H], X) :- pertenece(d(X,Y,H), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [], Y). % Caso sin epsilon intermedio, 2+ elementos (3+ originales)
reconoce_eps_siguiente(af(Q,S,F,Delta), [H], X) :- pertenece(d(X,Y,epsilon), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [H], Y). % Caso con epsilon intermedio, 2+ elementos (3+ originales)
% -- Tira con más de un simbolo por consumir
reconoce_eps_siguiente(af(Q,S,F,Delta), [H|T], X) :- pertenece(d(X,Y,H), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), T, Y).
reconoce_eps_siguiente(af(Q,S,F,Delta), [H|T], X) :- pertenece(d(X,Y,epsilon), Delta), reconoce_eps_siguiente(af(Q,S,F,Delta), [H|T], Y).
% reconoce_eps_final controla si se consumió toda la tira pero hay un camino de largo finito hasta un estado final a través de transiciones epsilon
reconoce_eps_final(af(_Q,_S,F,[d(X,Y,epsilon)|T]), X) :- pertenece(Y,F). % Caso 1 epsilon al final
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
simbolos([H]) :- H\=epsilon.
simbolos([H|T]) :- H\=epsilon, no_pertenece(H,T), simbolos(T).

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

% es_transicion_determinista(+Lista)
% Comprueba que una serie de transiciones conforman las aristas de un AFD (no hay 2 transiciones conmismo origen y Simbolo de transición)
es_transicion_determinista([_]).
es_transicion_determinista([d(O, _D, S)|T]) :-
  origenesSimbolos(T, OrigenesSimbolos),
  no_pertenece(os(O,S), OrigenesSimbolos).


% origenesSimbolos(+Transiciones, ?OrigenesSimbolos)
% Mapea la lista Transiciones a la lista de tuplas OrigenesSimbolos con forma os(Origen, Simbolo)
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
