:- module(core,
[
  % PREDICADOS PRINCIPALES
  movimiento/5, % +Casa, +Tablero, +Jugador, -NuevoTablero, -Final
  % Realiza el movimiento producto de sacar las semillas de Casa y repartirlas en las siguientes para el turno correspondiente al Jugador
  recoger_semillas/8, % +CasilleroFinal, +Jugador, +Tablero, -NuevoTablero, +Score1, -NuevoScore1, +Score2, -NuevoScore2 
  % Realiza la recogida producto de levantar las semillas en CasilleroFinal, devolviendo los puntajes y el tablero actualizado
  comprobar_validez/1, % +Tablero
  % Comprueba si el tablero actual es valido dada la jugada anterior
  quedan_movimientos_validos/2, % +Turno, +Tablero
  % Comprueba si quedan movimientos validos en el tablero actual para el jugador Turno
  terminar_partida_invalida/5 %+Score1, +Score2, +Tablero, -NuevoScore1, -NuevoScore2
  % Suma puntajes finales de una partida sin jugadas validas restantes
]).

:- use_module(utils).

% --------------------------
% Predicados principales
% --------------------------

movimiento(Casa, Tablero, jugador1, NuevoTablero, Final) :-
    convertir_a_indice(Casa, Indice),
    Indice < 6,
    nth0(Indice, Tablero, Semillas),
    Semillas > 0,
    reemplazar(Indice, 0, Tablero, TableroActual),
    repartir_semillas(Indice, TableroActual, NuevoTablero, Semillas, Final).
movimiento(Casa, Tablero, jugador2, NuevoTablero, Final) :-
    convertir_a_indice(Casa, Indice),
    Indice >= 6,
    nth0(Indice, Tablero, Semillas),
    Semillas > 0,
    reemplazar(Indice, 0, Tablero, TableroActual),
    repartir_semillas(Indice, TableroActual, NuevoTablero, Semillas, Final).

recoger_semillas(CasilleroFinal, jugador1, Tablero, NuevoTablero, Score1, NuevoScore1, Score2, Score2) :-
    nth0(CasilleroFinal, Tablero, NumeroSemillas),
    (
        (CasilleroFinal > 5, NumeroSemillas > 1, NumeroSemillas < 4)
        ->
        (
            ScoreAux1 is Score1 + NumeroSemillas,
            NuevoCasilleroFinal is (CasilleroFinal - 1) mod 11,
            reemplazar(CasilleroFinal, 0, Tablero, TableroAux),
            recoger_semillas(NuevoCasilleroFinal, jugador1, TableroAux, NuevoTablero, ScoreAux1, NuevoScore1, Score2, Score2)
        )
        ;
        (
          NuevoScore1 is Score1,
          NuevoTablero = Tablero
        )
    ).
recoger_semillas(CasilleroFinal, jugador2, Tablero, NuevoTablero, Score1, Score1, Score2, NuevoScore2) :-
    nth0(CasilleroFinal, Tablero, NumeroSemillas),
    (
        (CasilleroFinal < 6, NumeroSemillas > 1, NumeroSemillas < 4)
        ->
        (
            ScoreAux2 is Score2 + NumeroSemillas,
            NuevoCasilleroFinal is (CasilleroFinal - 1) mod 11,
            reemplazar(CasilleroFinal, 0, Tablero, TableroAux),
            recoger_semillas(NuevoCasilleroFinal, jugador2, TableroAux, NuevoTablero, Score1, Score1, ScoreAux2, NuevoScore2)
        )
        ;
        (
          NuevoScore2 is Score2,
          NuevoTablero = Tablero
        )
    ).

comprobar_validez(Tablero) :-
    separar_tablero(Tablero, Casas1, Casas2),
    colapsar(Casas1, Semillas1),
    colapsar(Casas2, Semillas2),
    colapsar(Tablero, SemillasTotales),
    (SemillasTotales =:= 0;
    (Semillas1 > 0, Semillas2 > 0)).

quedan_movimientos_validos(jugador1, Tablero) :-
    separar_tablero(Tablero, Casas1, _),
    colapsar(Casas1, SemillasJugador),
    SemillasJugador > 0.

quedan_movimientos_validos(jugador2, Tablero) :-
    separar_tablero(Tablero, _, Casas2),
    colapsar(Casas2, SemillasJugador),
    SemillasJugador > 0.

terminar_partida_invalida(Score1, Score2, Tablero, NuevoScore1, NuevoScore2) :-
    separar_tablero(Tablero, Casas1, Casas2),
    colapsar(Casas1, Semillas1),
    colapsar(Casas2, Semillas2),
    NuevoScore1 is Semillas1 + Score1,
    NuevoScore2 is Semillas2 + Score2.

% --------------------------
% Predicados auxiliares
% --------------------------

repartir_semillas(Final, Tablero, NuevoTablero, 0, Final) :-
    nth0(Final, Tablero, Semillas),
    reemplazar(Final, Semillas, Tablero, NuevoTablero).
repartir_semillas(Indice, Tablero, NuevoTablero, Restantes, Final) :-
    Restantes > 0,
    NuevoIndice is Indice + 1,
    ModuleIndice is NuevoIndice mod 12,
    NuevoRestantes is Restantes - 1,
    nth0(ModuleIndice, Tablero, Semillas),
    MasUnaSemilla is Semillas + 1,
    reemplazar(ModuleIndice, MasUnaSemilla, Tablero, TableroActual),
    repartir_semillas(ModuleIndice, TableroActual, NuevoTablero, NuevoRestantes, Final).



