% Import library(lists) to use max_list/2 and min_list/2
:- use_module(library(lists)).

% List of possible moves
moves(['up', 'down', 'left', 'right']).

mejor_movimiento(Tablero, NivelMiniMax, Estrategia, Jugada) :-
    Estrategia = ia,
    moves(Moves),
    findall([Move, NewBoard], (member(Move, Moves), movimientoT(Tablero, Move, NewBoard, _)), Successors),
    (
        Successors \= [], !, % Make sure that there are available moves
        best(Successors, NivelMiniMax, _, Jugada)
    ;
        % If there are no successors, select 'up' as the default move
        Jugada = up
    ).

% Best move based on alpha-beta pruning
best([Move], _, Move, Move).
best([Move1 | Rest], Depth, Alpha, Best) :-
    alphabeta(Move1, Depth, -inf, inf, Alpha1, _),
    best(Rest, Depth, Alpha, Best1),
    better_of(Move1, Alpha1, Best1, Alpha, Best).

better_of(Move0, Alpha0, _, Alpha, Move) :-
    Alpha0 > Alpha, !, Move = Move0.
better_of(_, _, Move1, Alpha, Move) :-
    Move = Move1.

% Alpha-beta pruning
alphabeta(Board, _, _, _, Score, _) :-
    terminal(Board), !, heuristic(Board, Score).
alphabeta(Board, Depth, Alpha, Beta, Score, BestSuccessor) :-
    Depth > 0,
    findall(NewBoard, (moves(Moves), member(Move, Moves), movimientoT(Board, Move, NewBoard, _)), Successors), 
    Successors \= [], !, % Make sure that there are available moves
    bounded_best(Successors, Depth, Alpha, Beta, Board, Score, BestSuccessor).
alphabeta(Board, _, _, _, Score, _) :-
    heuristic(Board, Score).

% Bounded best
bounded_best([Board1 | Boards], Depth, Alpha, Beta, _, Best, BestBoard) :-
    Alpha1 is -Beta, % max value
    Beta1 is -Alpha, % min value
    Depth1 is Depth - 1,
    alphabeta(Board1, Depth1, Alpha1, Beta1, Val1, _),
    Value is -Val1,
    bounded_best(Boards, Depth, Alpha, Beta, Board1, Value, BestBoard, Best).
bounded_best([], _, Alpha, _, Board, Alpha, Board).

bounded_best([Board1 | Boards], Depth, Alpha, Beta, Board, Value, BestBoard, Best) :-
    prune(Board1, Boards, Depth, Alpha, Beta, Board, Value, BestBoard, Best).

% Pruning
prune(Board, _, Depth, Alpha, Beta, _, Value, BestBoard, Best) :-
    Value >= Beta, !, % Beta cut-off
    Best = Board,
    BestBoard = Value.
prune(Board, Boards, Depth, Alpha, Beta, _, _, BestBoard, Best) :-
    Alpha1 is max(Alpha, Value),
    bounded_best(Boards, Depth, Alpha1, Beta, Board, BestBoard, Best).

% Check if the current state is a terminal state (i.e., no further moves can be made)
terminal(Board) :-
    \+ can_move(Board).

% Check if a move can be made from the current state
can_move(Board) :-
    moves(Moves),
    member(Move, Moves),
    movimientoT(Board, Move, NewBoard, _),
    \+ same_board(Board, NewBoard).

% Check if two board states are the same
same_board(Board1, Board2) :-
    flatten_board(Board1, FlatBoard1),
    flatten_board(Board2, FlatBoard2),
    FlatBoard1 == FlatBoard2.

% Helper function to flatten the board
flatten_board(Board, FlatBoard) :-
    Board =.. [_|Rows],
    maplist([f(A,B,C,D), [A,B,C,D]]>>true, Rows, Matrix),
    append(Matrix, FlatBoard).

% Free Tiles heuristic
free_tiles(m(R1,R2,R3,R4), FreeTiles) :-
    findall(X, (member(R, [R1, R2, R3, R4]), arg(_, R, X), X = '-'), FreeCells),
    length(FreeCells, FreeTiles).

% Smoothness heuristic
smoothness(m(R1,R2,R3,R4), Smoothness) :-
    findall(Diff, (member(R, [R1, R2, R3, R4]), row_smoothness(R, Diff)), Diffs),
    sum_list(Diffs, Smoothness).

row_smoothness(Row, Smoothness) :-
    arg(1, Row, Cell1),
    arg(2, Row, Cell2),
    arg(3, Row, Cell3),
    arg(4, Row, Cell4),
    calc_diff(Cell1, Cell2, D1),
    calc_diff(Cell2, Cell3, D2),
    calc_diff(Cell3, Cell4, D3),
    Smoothness is D1 + D2 + D3.

calc_diff('-', _, 0).
calc_diff(_, '-', 0).
calc_diff(X, Y, Diff) :- number(X), number(Y), Diff is abs(X - Y).

% Heuristic calculation
heuristic(Tablero, Score) :-
    free_tiles(Tablero, F),
    smoothness(Tablero, S),
    Score is F + S.

mejor_movimiento(Tablero, NivelMiniMax, ia, Jugada) :-
    alphabeta(Tablero, NivelMiniMax, -inf, inf, max, _, _, Jugada).

% +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

% Inicio de predicado movimientoT
movimientoT(Tablero, Movimiento, TableroNew, ScoreGen) :-
    once(aux_left(Tablero, Movimiento, TableroNew, ScoreGen));
	once(aux_up(Tablero, Movimiento, TableroNew, ScoreGen));
	once(aux_right(Tablero, Movimiento, TableroNew, ScoreGen));
	once(aux_down(Tablero, Movimiento, TableroNew, ScoreGen)).

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
	rotartableroizq(Tablero, Tnew1),
	moverizquierda(Tnew1, Tnew2, ScoreGen),
	rotartableroder(Tnew2, TableroNew).

moverabajo(Tablero, TableroNew, ScoreGen):-
	rotartableroder(Tablero, Tnew1),
	moverizquierda(Tnew1, Tnew2, ScoreGen),
	rotartableroizq(Tnew2, TableroNew).

moverderecha(Tablero, TableroNew, ScoreGen):-
	rotartableroizq(Tablero, Tnew1),
	rotartableroizq(Tnew1, Tnew2),
	moverizquierda(Tnew2, Tnew3, ScoreGen),
	rotartableroder(Tnew3, Tnew4),
	rotartableroder(Tnew4, TableroNew).

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

%%%%%%%%%%%%%%%%%%%%%%%%% Test %%%%%%%%%%%%%%%%%%%%%%%%%

% testPlayGame plays a game using mejor_movimiento and returns the final score and a flag indicating whether the game was won
testPlayGame(Board, Won, Strategy) :-
    (testGameFull(Board) -> 
		Won = false;
    	mejor_movimiento(Board, _, Strategy, Move),
		movimientoT(Board, Move, NewBoard, _),
    	(testGameWon(NewBoard) -> 
			Won = true;
			add_next_number(NewBoard, FullNewBoard),
        	testPlayGame(FullNewBoard, Won, Strategy)
		)
	).

% Add a new number to the board
add_next_number(Grid, NewGrid) :-
    repeat,
    random_between(1, 4, Row),
    random_between(1, 4, Col),
    arg(Row, Grid, RowTerm),
    arg(Col, RowTerm, Cell),
    (Cell == '-' -> 
		random(X),
		(X < 0.8 -> Number = 2 ; Number = 4),
		setarg(Row, Grid, RowTerm),
		setarg(Col, RowTerm, Number),
		NewGrid = Grid,
		!;
		add_next_number(Grid, NewGrid)
	).    

% Dummy predicate to check if the game has been won (a 2048 tile has been reached)
testGameWon(m(f(A, B, C, D),
			  f(E, F, G, H),
			  f(I, J, K, L),
			  f(M, N, O, P))) :- member('2048', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]).

% Dummy predicate to check if the game board is full
testGameFull(m(f(A, B, C, D),
               f(E, F, G, H),
               f(I, J, K, L),
               f(M, N, O, P))) :- \+ member('-', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]).

empty_board(m(f('-', '-', '-', '-'),
              f('-', '-', '-', '-'),
              f('-', '-', '-', '-'),
              f('-', '-', '-', '-'))).

initial_board(Initial_Board) :-
	empty_board(Empty_Board),
	add_next_number(Empty_Board, Board),
	add_next_number(Board, Initial_Board).

% test/0 generates test data for the game
test :- 
	open('2048_test_data.txt', write, Stream),
	test(1, Stream),
    close(Stream).

% test/1 generate 10 initial boards and plays 100 games for each board
test(11, _).
test(Counter, Stream) :-
	initial_board(Board),
	write(Stream,'Testing board: '),
	write(Stream, Counter),
	write(Stream, ' '),
	write(Stream, Board),
	nl(Stream),
	testLoop(1, Stream, Board, 'ai'),
	Counter1 is Counter + 1,
	test(Counter1, Stream).

% testLoop play 100 games with the same initial board and writes the results to a file
testLoop(101, _, _, _).
testLoop(Counter, Stream, Board, Strategy) :-
    testPlayGame(Board, Won, Strategy),
    write(Stream, Counter),
    write(Stream, ' '),
	(Won -> write(Stream, 'Won') ; write(Stream, 'Lost')),
    nl(Stream),
    Counter1 is Counter + 1,
    testLoop(Counter1, Stream, Board, Strategy).

