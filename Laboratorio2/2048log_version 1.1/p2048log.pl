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

% Helper function to flatten the board
flatten_board(Board, FlatBoard) :-
    Board =.. [_|Rows],
    maplist([f(A,B,C,D), [A,B,C,D]]>>true, Rows, Matrix),
    append(Matrix, FlatBoard).

% measure the smoothness of the board
measure_smoothness(Board, Smooth) :-
    flatten_board(Board, FlatBoard),
    findall(Diff, (nth1(I, FlatBoard, Cell1), nth1(J, FlatBoard, Cell2), J is I+1, (integer(Cell1), integer(Cell2) -> (Cell1 =:= Cell2 -> Diff = 0 ; Diff is 1/(1+abs(Cell1-Cell2))) ; Diff=1)), Diffs),
    sum_list(Diffs, Smooth).

% edge scoring heuristic
edge_score(Board, Score) :-
    weights(WeightMatrix),
    flatten_board(Board, FlatBoard),
    pairs_keys_values(Pairs, FlatBoard, WeightMatrix),
    findall(Product, (member(Pair, Pairs), (Pair=(-,_) -> Product=0 ; Pair=(V,W) -> Product is V*W)), Products),
    sum_list(Products, Score).

% board evaluation function
evaluate_board(Board, Score) :-
    measure_smoothness(Board, SmoothnessScore),
    edge_score(Board, EdgeScore),
    Score is SmoothnessScore + EdgeScore.

% measure the smoothness of the board
measure_smoothness(Board, Smooth) :-
    findall(Diff, (arg(_, Board, Row), arg(_, Row, Cell1), arg(_, Row, Cell2), (integer(Cell1), integer(Cell2) -> Diff is 1/(1+abs(Cell1-Cell2)) ; Diff=1)), Diffs),
    sum_list(Diffs, Smooth).

% weights defines a weight matrix that favors positions in the top-right corner
weights([0.135759, 0.121925, 0.102812, 0.099937,
         0.0997992,0.088848, 0.076711, 0.0724143,
         0.060654, 0.0562579,0.037116, 0.0161889,
         0.0125498,0.00992495,0.00575871,0.00335193]).

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

% main predicate for choosing the best move
mejor_movimiento(Tablero, _, Estrategia, Jugada) :-
    strategy(Estrategia, Tablero, Jugada),
    !.
mejor_movimiento(_, _, _, up).

% Inicio de predicado movimientoT
movimientoT(Tablero, left, TableroNew, ScoreGen) :-
    once(aux_left(Tablero, left, TableroNew, ScoreGen)).

movimientoT(Tablero, up, TableroNew, ScoreGen) :-
    once(aux_up(Tablero, up, TableroNew, ScoreGen)).

movimientoT(Tablero, right, TableroNew, ScoreGen) :-
    once(aux_right(Tablero, right, TableroNew, ScoreGen)).

movimientoT(Tablero, down, TableroNew, ScoreGen) :-
    once(aux_down(Tablero, down, TableroNew, ScoreGen)).

aux_up(Tablero, up, TableroNew, ScoreGen) :-
    tolist(Tablero, Tnew),
    moverArriba(Tnew, Tsol, ScoreGen),
    reconstruir_tablero(Tsol,TableroNew).

aux_left(Tablero, left, TableroNew, ScoreGen) :-
    tolist(Tablero, Tnew),
    moverizquierda(Tnew, Tsol, ScoreGen),
    reconstruir_tablero(Tsol,TableroNew).

aux_right(Tablero, right, TableroNew, ScoreGen) :-
    tolist(Tablero, Tnew),
    moverderecha(Tnew, Tsol, ScoreGen),
    reconstruir_tablero(Tsol,TableroNew).

aux_down(Tablero, down, TableroNew, ScoreGen) :-
    tolist(Tablero, Tnew),
    moverabajo(Tnew, Tsol, ScoreGen),
    reconstruir_tablero(Tsol,TableroNew).

reconstruir_tablero([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], 
                  Tablero):-
    TermA =.. [f, A1, A2, A3, A4],
    TermB =.. [f, B1, B2, B3, B4],
    TermC =.. [f, C1, C2, C3, C4],
    TermD =.. [f, D1, D2, D3, D4],
    Tablero =.. [m, TermA, TermB, TermC, TermD].

tolist(Tablero, Lista) :-
    Tablero =.. [ _, A1, A2, A3, A4],
    A1 =.. [_ | Args1],
    A2 =.. [_ | Args2],
    A3 =.. [_ | Args3],
    A4 =.. [_ | Args4],
    append(Args1, Args2, Args12),
    append(Args12, Args3, Args123),
    append(Args123, Args4, Lista).
    
% Rotar el tablero hacia la derecha
rotartableroder([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],
            [E1,E2,E3,E4,F1,F2,F3,F4,G1,G2,G3,G4,H1,H2,H3,H4]) :-
	E1 = D1,
	E2 = C1,
	E3 = B1,
	E4 = A1,
	F1 = D2,
	F2 = C2,
	F3 = B2,
	F4 = A2,
	G1 = D3,
	G2 = C3,
	G3 = B3,
	G4 = A3,
	H1 = D4,
	H2 = C4,
	H3 = B4,
	H4 = A4.

% Rotar el tablero hacia la izquierda
rotartableroizq([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],
           [E1,E2,E3,E4,F1,F2,F3,F4,G1,G2,G3,G4,H1,H2,H3,H4]) :-
	E1 = A4,
	E2 = B4,
	E3 = C4,
	E4 = D4,
	F1 = A3,
	F2 = B3,
	F3 = C3,
	F4 = D3,
	G1 = A2,
	G2 = B2,
	G3 = C2,
	G4 = D2,
	H1 = A1,
	H2 = B1,
	H3 = C1,
	H4 = D1.

moverArriba(Tablero, TableroNew, ScoreGen):-
	rotartableroizq(Tablero, Temp1),
	moverizquierda(Temp1, Temp2, ScoreGen),
	rotartableroder(Temp2, TableroNew).

moverabajo(Tablero, TableroNew, ScoreGen):-
	rotartableroder(Tablero, Temp1),
	moverizquierda(Temp1, Temp2, ScoreGen),
	rotartableroizq(Temp2, TableroNew).

moverderecha(Tablero, TableroNew, ScoreGen):-
	rotartableroizq(Tablero, Temp1),
	rotartableroizq(Temp1, Temp2),
	moverizquierda(Temp2, Temp3, ScoreGen),
	rotartableroder(Temp3, Temp4),
	rotartableroder(Temp4, TableroNew).

moverizquierda([], [], 0).

% caso: 2|2|2|2 -> 4|4|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X3 \= '-',
	X1 == X2,
	X3 == X4,
	N1 is X1 + X2,
	N2 is X3 + X4,
	N3 = '-',
	N4 = '-',
	moverizquierda(X, N, Acc_ScoreGen),
    ScoreGen is N1 + N2 + Acc_ScoreGen.

% caso: 2|2|2|4 -> 4|2|4|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X1 == X2,
	X2 == X3,
	N1 is X1 + X2,
	N2 = X3,
	N3 = X4,
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is N1 + Acc_ScoreGen.

% caso: 2|2|4|8 -> 4|4|8|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X1 == X2,
	X3 \= '-',
	N1 is X1 + X2,
	N2 = X3,
	N3 = X4,
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is N1 + Acc_ScoreGen.

% caso: 2|4|8|8 -> 2|4|16|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 \= '-',
	X1 \= X2,
	X2 \= X3,
	X3 == X4,
	N1 = X1,
	N2 = X2,
    (X3 == '-' ->  (N3 = X3, Aux_ScoreGen = 0); (N3 is X3 + X4, Aux_ScoreGen = N3)),
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is Aux_ScoreGen + Acc_ScoreGen.

% caso: 2|4|4|8 -> 2|8|8|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 \= '-',
	X2 == X3,
	N1 = X1,
	N2 is X2 + X3,
	N3 = X4,
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is N2 + Acc_ScoreGen.

% caso: -|-|-|2 -> 2|-|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 == '-',
	X2 == '-',
	X3 == '-',
	N1 = X4,
	N2 = '-',
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, ScoreGen).

% caso: -|-|2|2 -> 4|-|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 == '-',
	X2 == '-',
	X3 == X4,
	(X3 == '-' ->  (N1 = X3, Aux_ScoreGen = 0); (N1 is X3 + X4, Aux_ScoreGen = N1)),
	N2 = '-',
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is Aux_ScoreGen + Acc_ScoreGen.

% caso: -|-|2|4 -> 2|4|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 == '-',
	X2 == '-',
	N1 = X3,
	N2 = X4,
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, ScoreGen).

% caso: -|2|-|2 -> 4|-|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 == '-',
	X3 == '-',
	X2 == X4,
    (X2 == '-' ->  (N1 = X2, Aux_ScoreGen = 0); (N1 is X2 + X4, Aux_ScoreGen = N1)),
	N2 = '-',
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is Aux_ScoreGen + Acc_ScoreGen.

% caso: -|2|-|4 -> 2|4|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 == '-',
	X3 == '-',
	N1 = X2,
	N2 = X4,
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, ScoreGen).

% caso: -|2|2|4 -> 4|4|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 == '-',
	X2 == X3,
    (X2 == '-' ->  (N1 = X2, Aux_ScoreGen = 0); (N1 is X2 + X3, Aux_ScoreGen = N1)),
	N2 = X4,
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is Aux_ScoreGen + Acc_ScoreGen.

% caso: -|2|4|8 -> 2|4|8|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 == '-',
	X2 \= '-',
	X3 \= '-',
	X2 \= X3,
	X3 \= X4,
	N1 = X2,
	N2 = X3,
	N3 = X4,
	N4 = '-',
	moverizquierda(X,N, ScoreGen).

% caso: -|2|4|4 -> 2|8|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 == '-',
	X2 \= '-',
	X3 == X4,
	N1 = X2,
    (X3 == '-' ->  (N2 = X3, Aux_ScoreGen = 0); (N2 is X3 + X4, Aux_ScoreGen = N2)),
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is Aux_ScoreGen + Acc_ScoreGen.

% caso: 2|-|4|4 -> 2|8|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 == '-',
	X3 == X4,
	N1 = X1,
	(X3 == '-' ->  (N2 = X3, Aux_ScoreGen = 0); (N2 is X3 + X4, Aux_ScoreGen = N2)),
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is Aux_ScoreGen + Acc_ScoreGen.

% caso: 2|-|2|4 -> 4|4|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 == '-',
	X1 == X3,
	N1 is X1 + X3,
	N2 = X4,
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is N1 + Acc_ScoreGen.

% caso: 2|-|4|8 -> 2|4|8|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 == '-',
	X3 \= '-',
	N1 = X1,
	N2 = X3,
	N3 = X4,
	N4 = '-',
	moverizquierda(X,N, ScoreGen).

% caso: 2|-|-|2 -> 4|-|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 == '-',
	X3 == '-',
	X1 == X4,
	N1 is X1 + X4,
	N2 = '-',
	N3 = '-',
	N4 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is N1 + Acc_ScoreGen.

% caso: 2|-|-|4 -> 2|4|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 == '-',
	X3 == '-',
	N1 = X1,
	N2 = X4,
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, ScoreGen).

% caso: 2|2|-|4 -> 4|4|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X1 == X2,
	X3 == '-',
	N1 is X1 + X2,
	N2 = X4,
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is N1 + Acc_ScoreGen.

% caso: 2|4|-|4 -> 2|8|-|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 \= '-',
	X1 \= X2,
	X3 == '-',
	X2 == X4,
	N1 = X1,
	N2 is X2 + X4,
	N3 = '-',
	N4 = '-',
	moverizquierda(X,N, Acc_ScoreGen),
    ScoreGen is N2 + Acc_ScoreGen.

% caso: 2|4|-|8 -> 2|4|8|-
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 \= '-',
	X3 == '-',
	N1 = X1,
	N2 = X2,
	N3 = X4,
	N4 = '-',
	moverizquierda(X,N, ScoreGen).

% caso: 2|4|8|16 -> 2|4|8|16
moverizquierda([X1,X2,X3,X4|X], [N1,N2,N3,N4|N], ScoreGen) :-
	X1 \= '-',
	X2 \= '-',
	X3 \= '-',
	N1 = X1,
	N2 = X2,
	N3 = X3,
	N4 = X4,
	moverizquierda(X,N, ScoreGen).

% Fin de predicado movimientoT
