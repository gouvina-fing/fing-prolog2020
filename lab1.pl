%! Ej 1.1 - Largo de una lista
largo([],0).
largo([_|T],L1):-largo(T,L2),L1 is L2+1.

%! Ej 1.2 - Elemento pertenece a lista
pertenece(X,[X|_]).
pertenece(X,[_|T]):-pertenece(X,T).

%! Ej 1.3 - Elemento no pertenece a lista
no_pertenece(_,[]).
no_pertenece(X,[Y]):-X\=Y.
no_pertenece(X,[Y|T]):-no_pertenece(X,T),X\=Y.