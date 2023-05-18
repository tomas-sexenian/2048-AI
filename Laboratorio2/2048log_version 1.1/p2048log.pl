% Base case: an empty list remains the same
shift_left([], []).

% Case with a leading zero: shift the rest of the list and append a zero at the end
shift_left([0|T], SL) :-
    shift_left(T, SL1),
    append(SL1, [0], SL).

% Case with a leading non-zero number followed by a zero: swap the number and the zero and shift the rest
shift_left([H1,0|T], SL) :-
    H1 \= 0,
    shift_left([H1|T], SL1),
    append([0], SL1, SL).

% Case with a leading non-zero number followed by another number: leave the first number and shift the rest
shift_left([H1,H2|T], SL) :-
    H1 \= 0,
    H2 \= 0,
    shift_left([H2|T], SL1),
    append([H1], SL1, SL).

% Case with a leading non-zero number and no following number: leave the number
shift_left([H], [H]).

% The merge_left predicate merges adjacent equal numbers in a list
merge_left([], []).
merge_left([H], [H]).
merge_left([H1, H2 | T], ML) :-
    H1 = H2,
    NewH is H1 + H2,
    merge_left(T, ML1),
    append([NewH], ML1, ML).
merge_left([H1, H2 | T], ML) :-
    H1 \= H2,
    merge_left([H2 | T], ML1),
    append([H1], ML1, ML).

% The rotate_left predicate rotates a 2D list 90 degrees to the left
rotate_left([[]|_], []).
rotate_left(M, [H|T]) :-
    maplist(nth1(1), M, H),
    maplist(nth0(1), M, M1),
    rotate_left(M1, T).

rotate_right(Board, BoardNew) :-
    rotate_left(Board, Board1),
    rotate_left(Board1, Board2),
    rotate_left(Board2, BoardNew).


convertir_tablero([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], m(f(A11, A22, A33, A44), f(B11, B22, B33, B44), f(C11, C22, C33, C44), f(D11, D22, D33, D44))):-
    convertir_guion(A1,A11),
    convertir_guion(A2,A22),
    convertir_guion(A3,A33),
    convertir_guion(A4,A44),
    convertir_guion(B1,B11),
    convertir_guion(B2,B22),
    convertir_guion(B3,B33),
    convertir_guion(B4,B44),
    convertir_guion(C1,C11),
    convertir_guion(C2,C22),
    convertir_guion(C3,C33),
    convertir_guion(C4,C44),
    convertir_guion(D1,D11),
    convertir_guion(D2,D22),
    convertir_guion(D3,D33),
    convertir_guion(D4,D44).

convertir_guion(0, '-').
convertir_guion(X, X).

noMoreMoves(Board) :-
	once(moveLeft(Board, X)),
	equal(Board, X),
	once(moveRight(Board, Y)),
	equal(Board, Y),
	once(moveUp(Board, Z)),
	equal(Board,Z),
	once(moveDown(Board, W)),
	equal(Board,W).

aux_left(Tablero, left, TableroNew, 0) :-
    convertir(Tablero, Tnew),
    moveLeft(Tnew, Tsol),
    convertir_tablero(Tsol,TableroNew).

% Predicado principal para el movimiento
movimientoT(Tablero, left, TableroNew, 0) :-
    \+noMoreMoves(Tablero),
    once(aux_left(Tablero, left, TableroNew, 0)).

aux_up(Tablero, up, TableroNew, 0) :-
    convertir(Tablero, Tnew),
    moveUp(Tnew, Tsol),
    convertir_tablero(Tsol,TableroNew).

movimientoT(Tablero, up, TableroNew, 0) :-
    \+noMoreMoves(Tablero),
    once(aux_up(Tablero, up, TableroNew, 0)).

aux_right(Tablero, right, TableroNew, 0) :-
    convertir(Tablero, Tnew),
    moveRight(Tnew, Tsol),
    convertir_tablero(Tsol,TableroNew).

movimientoT(Tablero, right, TableroNew, 0) :-
    \+noMoreMoves(Tablero),
    once(aux_right(Tablero, right, TableroNew, 0)).

aux_down(Tablero, down, TableroNew, 0) :-
    convertir(Tablero, Tnew),
    moveDown(Tnew, Tsol),
    convertir_tablero(Tsol,TableroNew).

movimientoT(Tablero, down, TableroNew, 0) :-
    \+noMoreMoves(Tablero),
    once(aux_down(Tablero, down, TableroNew, 0)).

convertir(m(A, B, C, D), Lista) :-
    convertir_fila(A, Lista1),
    convertir_fila(B, Lista2),
    convertir_fila(C, Lista3),
    convertir_fila(D, Lista4),
    append(Lista1, Lista2, Temp1),
    append(Temp1, Lista3, Temp2),
    append(Temp2, Lista4, Lista).

convertir_fila(f(A, B, C, D), [A1, B1, C1, D1]):-
    remplazar_vacios(A,A1),
    remplazar_vacios(B,B1),
    remplazar_vacios(C,C1),
    remplazar_vacios(D,D1).
    
remplazar_vacios('-', 0).
remplazar_vacios(X, X).

% checks if two lists are equal
equal([],[]).
equal([H1|T1],[H2|T2]) :-
	H1 == H2,
	equal(T1,T2).

% rotate the board to the right
rotateRight([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],[E1,E2,E3,E4,F1,F2,F3,F4,G1,G2,G3,G4,H1,H2,H3,H4]) :-
	E1 is D1,
	E2 is C1,
	E3 is B1,
	E4 is A1,
	F1 is D2,
	F2 is C2,
	F3 is B2,
	F4 is A2,
	G1 is D3,
	G2 is C3,
	G3 is B3,
	G4 is A3,
	H1 is D4,
	H2 is C4,
	H3 is B4,
	H4 is A4.

% rotate the board to the left
rotateLeft([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],[E1,E2,E3,E4,F1,F2,F3,F4,G1,G2,G3,G4,H1,H2,H3,H4]) :-
	E1 is A4,
	E2 is B4,
	E3 is C4,
	E4 is D4,
	F1 is A3,
	F2 is B3,
	F3 is C3,
	F4 is D3,
	G1 is A2,
	G2 is B2,
	G3 is C2,
	G4 is D2,
	H1 is A1,
	H2 is B1,
	H3 is C1,
	H4 is D1.

% only moveLeft is actually implemented. Every other move just rotates
% the board first and then uses moveLeft.
moveUp(Board, NewBoard):-
	rotateLeft(Board, Temp1),
	moveLeft(Temp1, Temp2),
	rotateRight(Temp2, NewBoard).

moveDown(Board, NewBoard):-
	rotateRight(Board, Temp1),
	moveLeft(Temp1, Temp2),
	rotateLeft(Temp2, NewBoard).

moveRight(Board, NewBoard):-
	rotateLeft(Board, Temp1),
	rotateLeft(Temp1, Temp2),
	moveLeft(Temp2, Temp3),
	rotateRight(Temp3, Temp4),
	rotateRight(Temp4, NewBoard).

moveLeft([], []).
% X|X|X|X -> 2X|2X|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X3 \= 0,
	X1 == X2,
	X3 == X4,
	N1 is X1 + X2,
	N2 is X3 + X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X, N).
% X|X|X|Y -> 2X|X|Y|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X1 == X2,
	X2 == X3,
	N1 is X1 + X2,
	N2 is X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).
% X|X|Y|Z -> 2X|Y|Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X1 == X2,
	X3 \= 0,
	N1 is X1 + X2,
	N2 is X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).
% X|Y|Z|Z -> X|Y|2Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 \= 0,
	X1 \= X2,
	X2 \= X3,
	X3 == X4,
	N1 is X1,
	N2 is X2,
	N3 is X3 + X4,
	N4 is 0,
	moveLeft(X,N).
% X|Y|Y|Z -> X|2Y|Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 \= 0,
	X2 == X3,
	N1 is X1,
	N2 is X2 + X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).
% 0|0|0|X -> X|0|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 == 0,
	X3 == 0,
	N1 is X4,
	N2 is 0,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% 0|0|X|X -> 2X|0|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 == 0,
	X3 == X4,
	N1 is X3 + X4,
	N2 is 0,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% 0|0|X|Y -> X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 == 0,
	N1 is X3,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% 0|X|0|X -> 2X|0|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X3 == 0,
	X2 == X4,
	N1 is X2 + X4,
	N2 is 0,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% 0|X|0|Y -> X|Y|0|)
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X3 == 0,
	N1 is X2,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% 0|X|X|Y -> 2X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 == X3,
	N1 is X2 + X3,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% 0|X|Y|Z -> X|Y|Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 \= 0,
	X3 \= 0,
	X2 \= X3,
	X3 \= X4,
	N1 is X2,
	N2 is X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).
% 0|X|Y|Y -> X|2Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 \= 0,
	X3 == X4,
	N1 is X2,
	N2 is X3 + X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% X|0|Y|Y -> X|2Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X3 == X4,
	N1 is X1,
	N2 is X3 + X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% X|0|X|Y -> 2X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X1 == X3,
	N1 is X1 + X3,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% X|0|Y|Z -> X|Y|Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X3 \= 0,
	N1 is X1,
	N2 is X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).
% X|0|0|X -> 2X|0|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X3 == 0,
	X1 == X4,
	N1 is X1 + X4,
	N2 is 0,
	N3 is 0,
	N4 is 0,
	N4 is 0,
	moveLeft(X,N).
% X|0|0|Y -> X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X3 == 0,
	N1 is X1,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% X|X|0|Y -> 2X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X1 == X2,
	X3 == 0,
	N1 is X1 + X2,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% X|Y|0|Y -> X|2Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 \= 0,
	X1 \= X2,
	X3 == 0,
	X2 == X4,
	N1 is X1,
	N2 is X2 + X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).
% X|Y|0|Z -> X|Y|Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 \= 0,
	X3 == 0,
	N1 is X1,
	N2 is X2,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).
% X|Y|Z|W -> X|Y|Z|W
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 \= 0,
	X3 \= 0,
	N1 is X1,
	N2 is X2,
	N3 is X3,
	N4 is X4,
	moveLeft(X,N).


% The 'random' strategy just picks a move at random
mejor_movimiento(_, _, random, Move) :-
    random_member(Move, [up, down, left, right]).

% The 'dummy' strategy picks the move that results in the highest immediate score
mejor_movimiento(Board, _, dummy, Move) :-
    findall(Score-Move, (member(Move, [up, down, left, right]), movimientoT(Board, Move, _, Score)), ScoresMoves),
    keysort(ScoresMoves, SortedScoresMoves),
    reverse(SortedScoresMoves, [_-Move|_]).

% The 'ia' strategy uses the minimax algorithm to select the best move
mejor_movimiento(Board, Depth, ia, Move) :-
    findall(Score-Move, (member(Move, [up, down, left, right]), movimientoT(Board, Move, BoardNew, _), minimax(BoardNew, Depth, Score)), ScoresMoves),
    keysort(ScoresMoves, SortedScoresMoves),
    reverse(SortedScoresMoves, [_-Move|_]).

% COMIENZO PREDICADO PARA PASAR DE "m" A LISTA DE LISTAS

% helper predicate to translate f predicate to a list
f_to_list(f(A,B,C,D), List) :- 
    translate(A, TA),
    translate(B, TB),
    translate(C, TC),
    translate(D, TD),
    List = [TA, TB, TC, TD].

% translate "-" to 0 and keeps numbers as they are
translate(-, 0).
translate(X, X) :- number(X).

% main predicate to convert m predicate to list of lists
m_to_lists(M, Lists) :-
    M =.. [_|Fs],
    maplist(f_to_list, Fs, Lists).

% FIN PREDICADO PARA PASAR DE "m" A LISTA DE LISTAS