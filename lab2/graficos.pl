% Predicados gráficos para la segunda tarea de 
% Programación Lógica 2020 - OwareLog
% 
% Este archivo no es entregable, así que no debe ser modificado, salvo
% para ayudar a depurar. A medida que se encuentren errores y se
% sugieran mejoras van a haber nuevas versiones.
%
%


:- module(graficos,
[
 gr_crear/2, % -Ventana +Botones
 % Devuelve un handle a la Ventana creada.
 % En botones viene una lista de boton(Nombre, Evento)
 % indicando los botones a crear.
 
 gr_destruir/1, % +Ventana
 % Cierra la ventana e invalida el handle.
 
 gr_dibujar_tablero/2, % +Ventana +Tablero
 % Redibuja el tablero
 % Tablero es la lista de cantidades de semillas en cada casa de la forma [a1,b1,..,f1,a2,b2,..,f2]

 gr_evento/2, % +Ventana ?Evento
 % Devuelve en Evento la acción del usuario, que puede ser 
 % click(Casa), salir o
 % el del boton accionado.

 gr_mensaje/2, % +Ventana +String
 % Muestra String en una ventanita auxiliar,
 % quedando la aplicación a la espera de que ésta se cierre.

 gr_pregunta/3, % +Ventana +Pregunta ?Respuesta
 % Muestra una ventanita conteniendo Pregunta
 % y un espacio para que el usuario ingrese Respuesta.
 % Se regresa del predicado cuando el usuario selecciona el botón.
 % El predicado falla si se cierra el dialogo.
 
 gr_opciones/4, % +Ventana +Pregunta +Opciones ?Respuesta
 % Muestra una ventanita conteniendo Pregunta
 % y un botón por cada elemento de Opciones,
 % para que elija el usuario.
 % Se regresa del predicado cuando el usuario selecciona un botón,
 % devolviendo el elegido en Respuesta
 % El predicado falla si se cierra el dialogo.

 gr_estado/2 , % +Ventana +NuevoEstado
 % Muestra el NuevoEstado de la partida en la parte inferior 
 % de la pantalla.

 gr_purgar/0
 % Cierra todas las ventanas que pueden haber quedado abiertas
 % por fallos del programa. Hack, usar solo en desarrollo.
]).


:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(autowin)).
:- use_module(library(dragdrop)).

%La clase root.
:- pce_begin_class(my_frame, frame).

variable(queue, any, both).
variable(image, image, both).
variable(dialog, dialog, both).

%Desactivamos que el usuario la cierre.
%En su lugar, mandamos un mensaje salir.
wm_delete(Self) :->
	get(Self, queue, Queue),
	thread_send_message(Queue, salir).

:- pce_end_class.

click_tablero(Q, Punto) :-
	get(Punto, y, Y),
	get(Punto, x, X),
    C is (X - 20) // 140,
    F is (Y - 20) // 140,
    pos2house(F,C,House),
	thread_send_message(Q, click(House)).

pos2house(0,0,f1).
pos2house(0,1,e1).
pos2house(0,2,d1).
pos2house(0,3,c1).
pos2house(0,4,b1).
pos2house(0,5,a1).
pos2house(1,0,a2).
pos2house(1,1,b2).
pos2house(1,2,c2).
pos2house(1,3,d2).
pos2house(1,4,e2).
pos2house(1,5,f2).

:- send(class(my_frame), record_instances).
gr_purgar :-
	get(class(my_frame), instances, I),
	send(I, for_all, message(@arg1, destroy)).

gr_crear(Frame, Botones) :-
	message_queue_create(Q),
	new(Frame, my_frame('OwareLog')),
	new(W, auto_sized_dialog),
	send(Frame, can_resize,	@off),
	forall(member(boton(Txt, Val), Botones),
	       send(W, append, 
		    button(Txt, 
			   message(@prolog,
				   thread_send_message,
				   prolog(Q),
				   prolog(Val))))),
	send(W, max_size, size(1000, 1200)),
	new(I, image(kind := pixmap)),
	new(B, bitmap(I)),
	send(B, recogniser, 
	     click_gesture(left, '', single, 
			      message(@prolog,
				      click_tablero,
				      prolog(Q),
                      @event?position))),
	send(W, append, B),
	send(W, append, label(reporter)),
	send(Frame, append, W),
	send(Frame, queue, prolog(Q)),
	send(Frame, image, I),
	send(Frame, dialog, W),
	gr_dimensiones(Frame),
	send(Frame, open).

gr_destruir(Ventana) :-
	get(Ventana, queue, Q),
	message_queue_destroy(Q),
	send(Ventana, destroy).

gr_dimensiones(Ventana) :-
	get(Ventana, image, I),
	send(I, resize, 880, 320),
	get(Ventana, dialog, W),
	send(W, redraw),
	!.

gr_dibujar(Ventana, X, Y, Imagen):-
    atom_concat(Imagen, '.gif', Arch),
	new(ImgArch,image(Arch)),
	get(Ventana, image, I),
	send(I, draw_in, bitmap(ImgArch), point(X, Y)),
	send(Ventana, flush),
	!.

gr_texto(Ventana,X,Y,Texto):-
	new(Text,text(Texto)),
	get(Ventana, image, I),
	send(I, draw_in, Text, point(X,Y)).

gr_dibujar_tablero(Ventana, Tablero):-
    gr_dibujar(Ventana, 0, 0, 'back'),
    gr_dibujar_casas(Ventana, 0, Tablero),
	!.

gr_dibujar_casas(_,_,[]).
gr_dibujar_casas(Ventana,I,[Semillas|Casas]):-
	I < 6,!,
	X is (5 - I)*140 + 20,
	gr_dibujar_casa(Ventana, X, 20, Semillas),
    I1 is I + 1,
	gr_dibujar_casas(Ventana,I1,Casas).
gr_dibujar_casas(Ventana,I,[Semillas|Casas]):-
	% I >= 6,
	X is (I mod 6)*140 + 20,
	gr_dibujar_casa(Ventana, X, 160, Semillas),
    I1 is I + 1,
	gr_dibujar_casas(Ventana,I1,Casas).

gr_dibujar_casa(Ventana,X,Y,Semillas):-
	Semillas >= 7,!,
	gr_dibujar(Ventana,X,Y,'seedsm'),
	Xt is X + 65,
	Yt is Y + 65,
    gr_texto(Ventana,Xt,Yt,Semillas).
gr_dibujar_casa(Ventana,X,Y,Semillas):-
	% Semillas < 7,
	atom_concat('seeds',Semillas,FileName),
	gr_dibujar(Ventana,X,Y,FileName).

gr_evento(Ventana, Input) :-
	get(Ventana, queue, Q),
	thread_get_message(Q, Aux),
	!,
	Input = Aux.

gr_mensaje(V, Texto) :-
	new(D, dialog('Mensaje')),
	send(D, transient_for, V),
	send(D, append, label(lab, Texto)),
	send(D, append, button(ok,
			       message(D, return, @nil))),
	send(D, default_button, ok), % Ok: default button
	(   get(D, confirm, _Answer) % This blocks!
	->  send(D, destroy)
	;   true
	).

gr_pregunta(V, Preg, Resp) :-
	new(D, dialog('Pregunta')),
	send(D, transient_for, V),
        send(D, append,
             label(lab, Preg)),
	send(D, append,
             new(TI, text_item('', ''))),
        send(D, append,
             button(ok, message(D, return,
                                TI?selection))),
        send(D, default_button, ok), % Ok: default button
        get(D, confirm, Answer),     % This blocks!
        send(D, destroy),
	Answer = Resp.

gr_opciones(V, Texto, Opciones, Resp) :-
	new(D, dialog('Opciones')),
	send(D, transient_for, V),
	send(D, append, label(lab, Texto)),
	forall(member(O, Opciones),
	       send(D, append, button(O,
			       message(D, return, O)))),
	get(D, confirm,Answer),
	send(D, destroy),
	Resp = Answer.

gr_estado(MV,NuevoEstado) :-
	send(MV, report, progress,'%s',NuevoEstado).
