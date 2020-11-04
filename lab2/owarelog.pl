:- use_module(graficos).

% Este archivo se provee como una guía para facilitar la implementación y 
% entender el uso de graficos.pl
% El contenido de este archivo se puede modificar.

% El predicado minimax_depth/1 define la recursión máxima a utilizar en el algoritmo minimax
minimax_depth(7).

% owarelog(+Jugador1,+Jugador2)
% Jugador1 y Jugador2 pueden ser los átomos humano o maquina.
owarelog(Jugador1,Jugador2) :-
    pce_image_directory(icons),
    gr_crear(Visual, [
             boton('Guardar',guardar),
             boton('Cargar',cargar),
             boton('Reiniciar',reiniciar),
             boton('Salir',salir)] % salir puede ser por el boton o por el click en la ventana
         ),
    iniciar_juego(Visual,Jugador1,Jugador2),
    !,
    gr_destruir(Visual).

iniciar_juego(Visual,Jugador1,Jugador2):-
    loop(Visual,'',Jugador1,Jugador2,jugador1,[4,4,4,4,4,4,4,4,4,4,4,4],0,0).

contrincante(jugador1,jugador2).
contrincante(jugador2,jugador1).

% --------------------------
% Loop principal
% --------------------------

loop(Visual,MsgCmd,Jugador1,Jugador2,Turno,Tablero,Score1,Score2) :-
    gr_dibujar_tablero(Visual,Tablero),
    sformat(Msg, '~w Jugador 1: ~w puntos. Jugador 2: ~w puntos. Turno de ~w', [MsgCmd,Score1,Score2,Turno]),
    gr_estado(Visual,Msg),
    gr_evento(Visual,E),
    process_command(E,Visual,Jugador1,Jugador2,Turno,Tablero,Score1,Score2).

process_command(click(Casa),Visual,Jugador1,Jugador2,Turno,Tablero,Score1,Score2):-
    sformat(MsgCmd, 'Click en: ~w.', [Casa]),
    contrincante(Turno,SiguienteTurno),
    loop(Visual,MsgCmd,Jugador1,Jugador2,SiguienteTurno,Tablero,Score1,Score2).
process_command(salir,Visual,Jugador1,Jugador2,Turno,Tablero,Score1,Score2):-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
    ->  true
    ;   loop(Visual,'',Jugador1,Jugador2,Turno,Tablero,Score1,Score2)
    ).
process_command(reiniciar,Visual,Jugador1,Jugador2,Turno,Tablero,Score1,Score2):-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
    ->  iniciar_juego(Visual,Jugador1,Jugador2)
    ;   loop(Visual,'',Jugador1,Jugador2,Turno,Tablero,Score1,Score2)
    ).
process_command(guardar,Visual,Jugador1,Jugador2,Turno,Tablero,Score1,Score2):-
    loop(Visual,'Guardar.',Jugador1,Jugador2,Turno,Tablero,Score1,Score2).
process_command(cargar,Visual,Jugador1,Jugador2,Turno,Tablero,Score1,Score2):-
    loop(Visual,'Cargar.',Jugador1,Jugador2,Turno,Tablero,Score1,Score2).