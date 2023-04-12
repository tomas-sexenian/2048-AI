pertenece(X, [X|_]).
pertenece(X, [_|Resto]) :- 
    pertenece(X, Resto).

no_pertenece(_, []).
no_pertenece(X, [Y|Resto]) :-
     X \= Y, no_pertenece(X, Resto).

elegir(X, [X|L], L).
elegir(X, [H|L], [H|R]) :- 
    elegir(X, L, R).

contenida([], _).
contenida([X|Resto], L2) :- 
    pertenece(X, L2), contenida(Resto, L2).

permutacion([], []).
permutacion([H|T], L2) :-
    elegir(H, L2, R),
    permutacion(T, R).

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

repetir(N, V, L) :- repetir(N, V, [], L).
repetir(0, _, AL, AL).
repetir(N, V, AL, L) :- N1 is N - 1, repetir(N1, V, [V|AL], L).

matriz(F, C, V, M) :- matriz(F, C, V, [], M).
matriz(0, _, _, AM, AM).
matriz(F, C, V, AM, M) :- F1 is F - 1, repetir(C, V, L), matriz(F1, C, V, [L|AM], M).

sustituir(L1, VV, VN, I, C, L2) :- sustituir(L1, VV, VN, I, C, [], L2).
sustituir([], _, _, _, _, AL2, L2) :- reverse(AL2, L2).
sustituir([X|L1s], _, _, _, 0, AL2, L2) :- sustituir(L1s, _, _, _, 0, [X|AL2], L2).
sustituir([VV|L1s], VV, VN, 0, C, AL2, L2) :- C1 is C - 1, sustituir(L1s, VV, VN, 0, C1, [VN|AL2], L2).
sustituir([X|L1s], VV, VN, I, C, AL2, L2) :-  I1 is I - 1, sustituir(L1s, VV, VN, I1, C, [X|AL2], L2).

% COMIENZO PREDICADO 2.1
insertar_mueble_posicion(M1, NumFila, NumColumna, D1, D2, Etiqueta, M2) :-
insert_rows(M1, NumFila, NumColumna, D1, D2, Etiqueta, M2).

insert_rows(M1, _, _, 0, _, _, M1).
insert_rows([H|T], NumFila, NumColumna, D1, D2, Etiqueta, [H2|M2]) :-
    D1 > 0,
    NumFila1 is NumFila - 1,
    (NumFila1 =:= -1 ->
    (sustituir(H, 0, Etiqueta, NumColumna, D2, H2),
    D11 is D1 - 1);
    (H2 = H, D11 = D1)),
    insert_rows(T, NumFila1, NumColumna, D11, D2, Etiqueta, M2).
% FIN PREDICADO 2.1

% COMIENZO PREDICADO 2.2
agregar_mueble(Filas, Columnas, M1, Largo, Ancho, Etiqueta, M2) :-
    (try_insert(M1, Filas, Columnas, Largo, Ancho, Etiqueta, M2);
    try_insert(M1, Filas, Columnas, Ancho, Largo, Etiqueta, M2)).

try_insert(M1, Filas, Columnas, D1, D2, Etiqueta, M2) :-
    between(0, Filas, Fila),
    between(0, Columnas, Columna),
    MaxFila is Fila + D1,
    MaxColumna is Columna + D2,
    MaxFila =< Filas,
    MaxColumna =< Columnas,
    is_space_free(M1, Fila, Columna, D1, D2),
    insertar_mueble_posicion(M1, Fila, Columna, D1, D2, Etiqueta, M2).

is_space_free(_, _, _, 0, _).
is_space_free(M, Fila, Columna, D1, D2) :-
    D1 > 0,
    nth0(Fila, M, Row),
    all_zeros(Row, Columna, D2),
    D11 is D1 - 1,
    Fila1 is Fila + 1,
    is_space_free(M, Fila1, Columna, D11, D2).

all_zeros(_, 0, 0).
all_zeros([0|T], Columna, Count) :-
    Columna >= 0,
    Count > 0,
    Count1 is Count - 1,
    Columna1 is Columna - 1,
    all_zeros(T, Columna1, Count1).
% FIN PREDICADO 2.2

% COMIENZO PREDICADO 2.3
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