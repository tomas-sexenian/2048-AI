pertenece(X, [X|_]).
pertenece(X, [_|Resto]) :- 
    pertenece(X, Resto).

no_pertenece(_, []).
no_pertenece(X, [Y|Resto]) :-
     X \= Y, no_pertenece(X, Resto).

elegir(X, [X|Tail], Tail).
elegir(X, [Head|Tail], [Head|R]) :-
    elegir(X, Tail, R).

contenida([], _).
contenida([X|Resto], L2) :- 
    pertenece(X, L2), contenida(Resto, L2).

% COMIENZO PREDICADO PERMUTACION

% perm_aux(InputList, Accumulator, Permutation)
perm_aux([], Acc, Acc).
perm_aux(L, Acc, P) :-
    elegir(X, L, L1),
    perm_aux(L1, [X|Acc], P).

% permutacion(InputList, Permutation)
permutacion(L, P) :-
    perm_aux(L, [], P).

% FIN PREDICADO PERMUTACION

suma([], 0).
suma([H|T], S) :-
    suma(T, S1),
    S is S1 + H.

minimo([H|T], M) :- 
    minimo(T, H, M).
minimo([], M, M).
minimo([H|T], C, M) :-
    H < C,
    minimo(T, H, M).
minimo([H|T], C, M) :-
    H >= C,
    minimo(T, C, M).

repetir(0, _, []).
repetir(N, Valor, [Valor|Lista]) :-
    N > 0,
    NewN is N - 1,
    repetir(NewN, Valor, Lista).

matriz(0, _, _, []).
matriz(_, 0, _, []).
matriz(F, C, V, [Row|M]) :-
    repetir(C, V, Row),
    NewF is F - 1,
    matriz(NewF, C, V, M).

sustituir([], _, _, _, Cantidad, []).
sustituir(L1, _, _, _, 0, L1) :- 
    L1 \= [].
sustituir([H|T], ValorViejo, ValorNuevo, Inicio, Cantidad, [H|L2]) :-
    Inicio > 0,
    NewInicio is Inicio - 1,
    sustituir(T, ValorViejo, ValorNuevo, NewInicio, Cantidad, L2).
sustituir([ValorViejo|T], ValorViejo, ValorNuevo, 0, Cantidad, [ValorNuevo|L2]) :-
    Cantidad > 0,
    NewCantidad is Cantidad - 1,
    sustituir(T, ValorViejo, ValorNuevo, 0, NewCantidad, L2).

% COMIENZO PREDICADO 2.1
insertar_mueble_posicion(M1, NumFila, NumColumna, D1, D2, Etiqueta, M2) :-
    check_zeros(M1, NumFila, NumColumna, D1, D2),
    insert_rows(M1, NumFila, NumColumna, D1, D2, Etiqueta, M2);
    check_zeros(M1, NumFila, NumColumna, D2, D1),
    insert_rows(M1, NumFila, NumColumna, D2, D1, Etiqueta, M2).

check_zeros(_, _, _, 0, _).
check_zeros([H|T], NumFila, NumColumna, D1, D2) :-
    D1 > 0,
    (   NumFila =< 0,
        check_row_zeros(H, NumColumna, D2),
        D11 is D1 - 1
    ;   NumFila > 0,
        D11 = D1
    ),
    NumFila1 is NumFila - 1,
    check_zeros(T, NumFila1, NumColumna, D11, D2).

check_row_zeros(_, _, 0).
check_row_zeros([H|T], NumColumna, D2) :-
    D2 > 0,
    (NumColumna =< 0 ->
    (H = 0,
    D21 is D2 - 1);
    D21 = D2),
    NumColumna1 is NumColumna - 1,
    check_row_zeros(T, NumColumna1, D21).

insert_rows(M1, _, _, 0, _, _, M1).
insert_rows([H|T], NumFila, NumColumna, D1, D2, Etiqueta, [H2|M2]) :-
    D1 > 0,
    (NumFila =< 0 ->
    (sustituir(H, 0, Etiqueta, NumColumna, D2, H2),
    D11 is D1 - 1);
    (H2 = H, D11 = D1)),
    NumFila1 is NumFila - 1,
    insert_rows(T, NumFila1, NumColumna, D11, D2, Etiqueta, M2).
% FIN PREDICADO 2.1

% COMIENZO PREDICADO 2.2
agregar_mueble(Filas, Columnas, M1, Largo, Ancho, Etiqueta, M2) :-
    Filas > 0,
    Columnas > 0,
    (   % Try Width x Length orientation
        MaxFila is Filas - Largo,
        MaxColumna is Columnas - Ancho,
        between(0, MaxFila, F),
        between(0, MaxColumna, C),
        insertar_mueble_posicion(M1, F, C, Largo, Ancho, Etiqueta, M2)
    ;
        % Try Length x Width orientation
        MaxFila is Filas - Ancho,
        MaxColumna is Columnas - Largo,
        between(0, MaxFila, F),
        between(0, MaxColumna, C),
        insertar_mueble_posicion(M1, F, C, Ancho, Largo, Etiqueta, M2)
    ).
% FIN PREDICADO 2.2

% COMIENZO PREDICADO 2.3

% ESTA IMPLEMENTACION PROBABLEMENTE SEA CUALQUIER COSA
% HABRIA QUE ARRANCAR ESTE PREDICADO DE 0

muebles(Filas, Columnas, Muebles, M) :-
    empty_matrix(Filas, Columnas, EmptyMatrix),
    arrange_muebles(Muebles, EmptyMatrix, M).

arrange_muebles([], M, M).
arrange_muebles([furniture(Largo, Ancho, Etiqueta)|T], M1, M) :-
    agregar_mueble(Filas, Columnas, M1, Largo, Ancho, Etiqueta, M2),
    arrange_muebles(T, M2, M).

empty_matrix(0, _, []).
empty_matrix(Filas, Columnas, [Row|M]) :-
    Filas > 0,
    create_row(Columnas, Row),
    Filas1 is Filas - 1,
    empty_matrix(Filas1, Columnas, M).

create_row(0, []).
    create_row(Columnas, [0|Row]) :-
    Columnas > 0,
    Columnas1 is Columnas - 1,
    create_row(Columnas1, Row).
% FIN PREDICADO 2.3