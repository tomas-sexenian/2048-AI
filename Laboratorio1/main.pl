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