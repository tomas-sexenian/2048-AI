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