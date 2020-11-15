:- module(maquina,
[
  % PREDICADOS GENERALES
  iniciar_maquina/6  % +Profundidad, +Turno, +Tablero, +Score1, +Score2, -CasaElegida
  % Utilizando el algoritmo minimax, la máquina toma una decisión en base a la información proporcionada
]).

:- use_module(core).
:- use_module(utils).

% --------------------------
% Predicados principales
% --------------------------

iniciar_maquina(Depth, Turno, Tablero, Score1, Score2, CasaElegida) :-
  devolver_peor_val_heuristica(jugador1, Alpha),
  cambiar_turno(Turno, NuevoTurno),
  devolver_peor_val_heuristica(jugador2, Beta),
  minimax(Depth, Turno, Tablero, Score1, Score2, Alpha, Beta, CasaElegida, _HeuristicValue).

% --------------------------
% Predicados auxiliares
% --------------------------

% Paso base
minimax(0, _Turno, _Tablero, Score1, Score2, _Alpha, _Beta, _CasaElegida, HeuristicValue) :-
  heuristic_value(Score1, Score2, HeuristicValue), !.
minimax(_Depth, _Turno, _Tablero, Score1, Score2, _Alpha, _Beta, _CasaElegida, HeuristicValue) :-
  terminal_node(Score1, Score2),
  heuristic_value(Score1, Score2, HeuristicValue), !.

% Paso Inductivo
minimax(Depth, Turno, Tablero, Score1, Score2, Alpha, Beta, CasaElegida, HeuristicValue) :-
  posibles_casas(Turno, Casas),
  mejor_casa(Casas, Depth, Turno, Tablero, Score1, Score2, Alpha, Beta, CasaElegida, HeuristicValue).

% Determina que casa retorna un mejor valor de minimax
mejor_casa([ Casa ], Depth, Turno, Tablero, Score1, Score2, Alpha, Beta, Casa, HeuristicValue) :-
  (
    realizar_movimiento(Casa, Tablero, Turno, Score1, Score2, NuevoScore1, NuevoScore2, NuevoTablero) ->
    (
      NewDepth is Depth - 1,
      cambiar_turno(Turno, NuevoTurno),
      minimax(NewDepth, NuevoTurno, NuevoTablero, NuevoScore1, NuevoScore2, Alpha, Beta, _, HeuristicValue)
    )
    ;
    (
      devolver_peor_val_heuristica(Turno, HeuristicValue)
    ),
    !
  ).

mejor_casa([ Casa| CasasRestantes ], Depth, Turno, Tablero, Score1, Score2, Alpha, Beta, CasaElegida, HeuristicValue) :-
  (
    realizar_movimiento(Casa, Tablero, Turno, Score1, Score2, NuevoScore1, NuevoScore2, NuevoTablero)
    ->
    (
      NewDepth is Depth - 1,
      cambiar_turno(Turno, NuevoTurno),
      minimax(NewDepth, NuevoTurno, NuevoTablero, NuevoScore1, NuevoScore2, Alpha, Beta, _, HeuristicValue1),
      asignar_nuevo_alpha_beta(Turno, Alpha, Beta, HeuristicValue1, NewAlpha, NewBeta),
      (
        Alpha >= Beta
        ->
        (
          CasaElegida = Casa,
          HeuristicValue = HeuristicValue1, !
        )
        ;
        (
          mejor_casa(CasasRestantes, Depth, Turno, Tablero, Score1, Score2, NewAlpha, NewBeta, Casa2, HeuristicValue2),
          elegir_mejor_casa(Casa2, HeuristicValue2, Casa, HeuristicValue1, Turno, CasaElegida, HeuristicValue) % En caso de empate elige la casa con movimiento valido
        )
      )
    )
    ;
    (
      mejor_casa(CasasRestantes, Depth, Turno, Tablero, Score1, Score2, Alpha, Beta, CasaElegida, HeuristicValue)
    )
  ).

%
asignar_nuevo_alpha_beta(Turno, Alpha, Beta, HeuristicValue, NewAlpha, Beta) :-
  is_maximizing(Turno),
  NewAlpha is max(HeuristicValue, Alpha), !.
asignar_nuevo_alpha_beta(Turno, Alpha, Beta, HeuristicValue, Alpha, NewBeta) :-
  \+is_maximizing(Turno),
  NewBeta is min(HeuristicValue, Beta).

% Elige una de las mejores casas en caso de haber empate
elegir_mejor_casa(Casa1, HeuristicValue1, _Casa2, HeuristicValue2, Turno, Casa1, HeuristicValue1) :-
  is_maximizing(Turno),
  HeuristicValue1 > HeuristicValue2, !
  ;
  \+is_maximizing(Turno),
  HeuristicValue1 < HeuristicValue2, !.


elegir_mejor_casa(_Casa1, _HeuristicValue1, Casa2, HeuristicValue2, Turno, Casa2, HeuristicValue2).

devolver_peor_val_heuristica(Turno, -9999) :-
  is_maximizing(Turno).

devolver_peor_val_heuristica(_Turno, 9999).

% Siempre maximiza el jugador1
is_maximizing(jugador1).

% Maneja el cambio de turno en predicciones
cambiar_turno(jugador1, jugador2).
cambiar_turno(jugador2, jugador1).

% Determina el valor de evaluación del minimax
heuristic_value(Score1, Score2, Val) :-
  Val is Score1 - Score2.

% Determina si el nodo actual en el árbol termina la partida
terminal_node(Score1, _Score2) :-
  Score1 > 24.
terminal_node(_Score1, Score2) :-
  Score2 > 24.
terminal_node(Score1, Score2) :-
  (Score1 is 24, Score2 is 24).

% Simula un movimiento
realizar_movimiento(Casa, Tablero, Turno, Score1, Score2, NuevoScore1, NuevoScore2, NuevoTablero2) :-
  movimiento(Casa, Tablero, Turno, NuevoTablero, CasilleroFinal),
  recoger_semillas(CasilleroFinal, Turno, NuevoTablero, NuevoTablero2, Score1, NuevoScore1, Score2, NuevoScore2),
  comprobar_validez(NuevoTablero2).

% Lista de casas donde cada jugador puede realizar movimientos
posibles_casas(jugador1, [a1, b1, c1, d1, e1, f1]).
posibles_casas(jugador2, [a2, b2, c2, d2, e2, f2]).