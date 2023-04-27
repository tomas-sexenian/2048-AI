% pertenece(?X,?L) ← El elemento X pertenece a la lista L.
pertenece(X, [X|_]).
pertenece(X, [_|Resto]) :- 
    pertenece(X, Resto).

% no_pertenece(+X,+L) ← El elemento X no pertenece a la lista L.
no_pertenece(_, []).
no_pertenece(X, [Y|Resto]) :-
     X \= Y, no_pertenece(X, Resto).

% elegir(?X,?L,?R) ← La lista R resulta de eliminar el elemento X de la lista L.
elegir(X, [X|Resto], Resto).
elegir(X, [Actual|Resto], [Actual|R]) :-
    elegir(X, Resto, R).

% contenida(+L1,+L2) ← Todos los elementos de L1 pertenecen a L2.
contenida([], _).
contenida([X|Resto], L2) :- 
    pertenece(X, L2), contenida(Resto, L2).

% perm_aux(+L, +Acc, ?P) ← La lista P es una permutación de la lista L y Acc es el acumulador que construye la permutacion P
perm_aux([], Acc, Acc).
perm_aux(L, Acc, P) :-
    elegir(X, L, L1),
    perm_aux(L1, [X|Acc], P).
% permutacion(+L1,?L2) ← La lista L2 es una permutación de la lista L1. 
permutacion(L, P) :-
    perm_aux(L, [], P).

% suma(+L,?S) ← S es la suma de los elementos de la lista L
suma([], 0).
suma([H|T], S) :-
    suma(T, S1),
    S is S1 + H.

% minimo(+L,?M) ← M es el minimo de los elementos de la lista L
minimo([H|T], M) :- 
    minimo(T, H, M).
% minimo(+L, +C, ?M) ← M es el minimo de los elementos de la lista L considerando C como el minimo inicial
minimo([], M, M).
minimo([H|T], C, M) :-
    H < C,
    minimo(T, H, M).
minimo([H|T], C, M) :-
    H >= C,
    minimo(T, C, M).

% repetir(+N,+Valor,?Lista) ← Lista es una lista que contiene el elemento Valor
repetir(0, _, []).
repetir(N, Valor, [Valor|Lista]) :-
    N > 0,
    NewN is N - 1,
    repetir(NewN, Valor, Lista).

% matriz(+F,+C,+V,?M) ← M es una matriz de F filas y C columnas, implementada como una lista de listas, donde todas las celdas tienen el valor V
matriz(0, _, _, []).
matriz(_, 0, _, []).
matriz(F, C, V, [Fila|M]) :-
    F > 0,
    C > 0,
    repetir(C, V, Fila),
    NewF is F - 1,
    matriz(NewF, C, V, M).

% sustituir(+L1,+ValorViejo,+ValorNuevo,+Inicio,+Cantidad,?L2)
sustituir([], _, _, _, _, []).
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

% insertar_mueble_posicion(+M1,+NumFila,+NumColumna,+D1,+D2,+Etiqueta,?M2) ←
% M2 es el resultado de insertar el mueble de dimensiones D1*D2 en la matriz M1,
% comenzando exactamente en la fila NumFila y la columna NumColumna (contar
% desde 0).
insertar_mueble_posicion(M1, NumFila, NumColumna, D1, D2, Etiqueta, M2) :-
    revisar_ceros(M1, NumFila, NumColumna, D1, D2),
    insertar_filas(M1, NumFila, NumColumna, D1, D2, Etiqueta, M2).

% revisar_ceros(+L, +NumFila, +NumColumna, +D1, +D2) ← A partir de la posicion (NumFila, NumColumna) de la matriz L, revisa que 
% haya un rectangulo de D1 ceros de largo y D2 ceros de ancho
revisar_ceros(_, _, _, 0, _).
revisar_ceros([H|T], NumFila, NumColumna, D1, D2) :-
    D1 > 0,
    (   NumFila =< 0,
        revisar_ceros_columna(H, NumColumna, D2),
        D11 is D1 - 1
    ;   NumFila > 0,
        D11 = D1
    ),
    NumFila1 is NumFila - 1,
    revisar_ceros(T, NumFila1, NumColumna, D11, D2).

% revisar_ceros_columna(+L, +NumColumna, +D2) ← Revisa que la lista L contenga D2 ceros a partir de la posicion NumColumna
revisar_ceros_columna(_, _, 0).
revisar_ceros_columna([H|T], NumColumna, D2) :-
    D2 > 0,
    NumColumna1 is NumColumna - 1,
    (   NumColumna =< 0,
        H = 0,
        D21 is D2 - 1,
        revisar_ceros_columna(T, NumColumna1, D21)
    ;   NumColumna > 0,
        D21 = D2,
        revisar_ceros_columna(T, NumColumna1, D21)
    ).

% insertar_filas(+M1, +NumFila, +NumColumna, +D1, +D2, +Etiqueta, ?M2) ← A partir de la posicion (NumFila, NumColumna) de la matriz M, sustituye todos los ceros 
% del rectangulo de D1 ceros de largo y D2 ceros de ancho por Etiqueta
insertar_filas(M1, _, _, 0, _, _, M1).
insertar_filas([H|T], NumFila, NumColumna, D1, D2, Etiqueta, [H2|M2]) :-
    D1 > 0,
    NumFila1 is NumFila - 1,
    (   NumFila =< 0,
        sustituir(H, 0, Etiqueta, NumColumna, D2, H2),
        D11 is D1 - 1,
        insertar_filas(T, NumFila1, NumColumna, D11, D2, Etiqueta, M2)
    ;   NumFila > 0,
        H2 = H, 
        D11 = D1,
        insertar_filas(T, NumFila1, NumColumna, D11, D2, Etiqueta, M2)
    ).
% FIN PREDICADO 2.1

% COMIENZO PREDICADO 2.2

% agregar_mueble(+Filas,+Columnas,+M1,+Largo,+Ancho,+Etiqueta,?M2) ← M2 es una
% forma de agregar el mueble de dimensiones Largo*Ancho en la matriz M1 de
% tamaño Filas y Columnas. Recordar que el mueble puede rotarse, por lo tanto
% tendrá dos formas de ubicarse según sus dimensiones, y además puede ubicarse
% en diferentes posiciones de la matriz siempre que el lugar esté libre.
agregar_mueble(Filas, Columnas, M1, Largo, Ancho, Etiqueta, M2) :-
    Filas > 0,
    Columnas > 0,
    (   % Try Width x Length orientation
        between(0, Filas, F),
        between(0, Columnas, C),
        insertar_mueble_posicion(M1, F, C, Largo, Ancho, Etiqueta, M2)
    ;
        % Try Length x Width orientation
        between(0, Filas, F),
        between(0, Columnas, C),
        insertar_mueble_posicion(M1, F, C, Ancho, Largo, Etiqueta, M2)
    ).
    
% FIN PREDICADO 2.2

% COMIENZO PREDICADO 2.3

% muebles(+Filas,+Columnas,+Muebles,?M) ← M es solución del problema de ubicar
% los muebles de la lista Muebles en una matriz de tamaño Filas y Columnas.
muebles(Filas, Columnas, Muebles, M) :-
    matriz(Filas, Columnas, 0, M1),
    agregar_muebles(Filas, Columnas, Muebles, M1, M2),
    M = M2.

% agregar_muebles(+Filas, +Columnas, +Muebles, +M1, ?M2) ← Itera la lista Muebles ubicando cada uno en la matriz M1 de dimensiones 
% Filas*Columnas, dando como resultado la matriz M2
agregar_muebles(_, _, [], M, M).
agregar_muebles(Filas, Columnas, [mueble(Largo, Ancho, V)|T], M1, M2) :-
    Largo > 0,
    Ancho > 0,
    agregar_mueble(Filas, Columnas, M1, Largo, Ancho, V, M3),
    agregar_muebles(Filas, Columnas, T, M3, M2).

% FIN PREDICADO 2.3