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


% list of possible moves
moves([up, down, left, right]).

% valid_move checks if a move changes the board
valid_move(Board, Move) :-
    movimientoT(Board, Move, NewBoard, _),
    Board \= NewBoard.

% random strategy
strategy(random, Board, Move) :-
    moves(Moves),
    random_member(Move, Moves),
    valid_move(Board, Move).

% dummy strategy
strategy(dummy, Board, Move) :-
    moves(Moves),
    findall(Score-Mv, (member(Mv, Moves), valid_move(Board, Mv), movimientoT(Board, Mv, _, Score)), ScoresMoves),
    max_member(_-Move, ScoresMoves).

% ia strategy
strategy(ia, Board, Move) :-
    moves(Moves),
    findall(Score-Mv, (member(Mv, Moves), valid_move(Board, Mv), movimientoT(Board, Mv, NewBoard, _), evaluate_board(NewBoard, Score)), ScoresMoves),
    pairs_keys_values(ScoresMoves, Scores, _),
    max_member(MaxScore, Scores),
    member(MaxScore-Move, ScoresMoves).

% evaluate board using a weight matrix, the highest tile value, the number of empty cells, and smoothness
evaluate_board(Board, Score) :-
    weights(Weights),
    board_values(Board, Values),
    maplist(product, Values, Weights, Products),
    sum_list(Products, WeightedSum),
    max_list(Values, Max),
    count_empty_cells(Board, Empty),
    measure_smoothness(Board, Smooth),
    Score is WeightedSum + 10*Max + 5*Empty + 50*Smooth.

% measure the smoothness of the board
measure_smoothness(Board, Smooth) :-
    findall(Diff, (arg(_, Board, Row), arg(_, Row, Cell1), arg(_, Row, Cell2), (integer(Cell1), integer(Cell2) -> Diff is 1/(1+abs(Cell1-Cell2)) ; Diff=1)), Diffs),
    sum_list(Diffs, Smooth).

% weights defines a weight matrix that favors positions in the top-right corner
weights([4,3,2,1,4,3,2,1,4,3,2,1,4,3,2,1]).

% product computes the product of two numbers
product(X, Y, Z) :- Z is X*Y.

% count the number of empty cells
count_empty_cells(Board, Count) :-
    findall(1, (arg(_, Board, Row), arg(_, Row, Cell), var(Cell)), Empties),
    length(Empties, Count).

% measure the monotonicity of the board
measure_monotonicity(Board, Mono) :-
    findall(Diff, (arg(_, Board, Row), arg(_, Row, Cell1), arg(_, Row, Cell2), (integer(Cell1), integer(Cell2) -> Diff is abs(Cell1-Cell2) ; Diff=0)), Diffs),
    sum_list(Diffs, Mono).

% board_values flattens the board to a list of values
board_values(Board, Values) :-
    findall(Value, 
            (
                arg(_, Board, Row), 
                arg(_, Row, Cell), 
                (integer(Cell) -> Value=Cell ; Value=0)
            ), 
            Values
    ).



% main predicate for choosing the best move
mejor_movimiento(Tablero, _, Estrategia, Jugada) :-
    strategy(Estrategia, Tablero, Jugada),
    !.
mejor_movimiento(_, _, _, up).

