%%%%%%%%%%%%%%%%%%%%%%%%% COMIENZO mejor_movimiento %%%%%%%%%%%%%%%%%%%%%%%%%

mejor_movimiento(Tablero, NivelMiniMax, Estrategia, Jugada) :-
    Estrategia = random, random_move(Tablero, Jugada), !.
mejor_movimiento(Tablero, NivelMiniMax, Estrategia, Jugada) :-
    Estrategia = dummy, dummy_move(Tablero, Jugada), !.
mejor_movimiento(Tablero, NivelMiniMax, Estrategia, Jugada) :-
    Estrategia = ia, ia_move(Tablero, NivelMiniMax, Jugada).

random_move(Tablero, Jugada) :-
    findall(Jugada, (member(Jugada, [up, down, left, right]), movimientoT(Tablero, Jugada, TableroNew, _), \+ Tablero = TableroNew), Jugadas),
    (Jugadas = [] -> Jugada = up ; random_member(Jugada, Jugadas)).

dummy_move(Tablero, Jugada) :-
    findall(Score-Jugada, (member(Jugada, [up, down, left, right]), movimientoT(Tablero, Jugada, TableroNew, Score), \+ Tablero = TableroNew), ScoreList),
    (ScoreList = [] -> Jugada = up ; (keysort(ScoreList, SortedScoreList), reverse(SortedScoreList, [_-Jugada|_]))).

ia_move(Tablero, NivelMiniMax, Jugada) :-
    findall(
        Score-Jugada, 
        (
            member(Jugada, [up, down, left, right]), 
            movimientoT(Tablero, Jugada, TableroNew, _), 
            minimax(TableroNew, NivelMiniMax, -inf, inf, Score), 
            \+ Tablero = TableroNew
        ), 
        ScoreList
    ),
    (ScoreList = [] -> Jugada = up ; (keysort(ScoreList, SortedScoreList), reverse(SortedScoreList, [_-Jugada|_]))).

% Minimax algorithm with Alpha-Beta pruning.
minimax(Tablero, 0, _, _, Score) :- !, evaluate(Tablero, Score).
minimax(Tablero, NivelMiniMax, Alpha, Beta, Score) :-
    NivelMiniMax > 0,
    NewNivel is NivelMiniMax - 1,
    findall(ScoreNew, 
            (member(Jugada, [up, down, left, right]), 
             movimientoT(Tablero, Jugada, TableroNew, _),
             minimax(TableroNew, NewNivel, Alpha, Beta, ScoreNew), 
             \+ Tablero = TableroNew), 
             Scores),
    (Scores = [] -> Score = -1 ; best_score(Scores, Alpha, Beta, NivelMiniMax, Score)).

% Best score helper function.
best_score([H|T], Alpha, Beta, Nivel, Best) :-
    best_score(T, Alpha, Beta, H, Nivel, Best).
best_score([], _, _, Best, _, Best).
best_score([H|T], Alpha, Beta, CurrentBest, Nivel, Best) :-
    update_bounds(Alpha, Beta, H, Nivel, NewAlpha, NewBeta),
    (NewAlpha >= NewBeta -> Best = H ; best_score(T, NewAlpha, NewBeta, CurrentBest, Nivel, Best)).

% Update bounds helper function.
update_bounds(Alpha, Beta, Score, Nivel, NewAlpha, Beta) :-
    Nivel mod 2 =:= 0, Score > Alpha, NewAlpha is Score, !.
update_bounds(Alpha, Beta, Score, Nivel, Alpha, NewBeta) :-
    Nivel mod 2 =:= 1, Score < Beta, NewBeta is Score, !.
update_bounds(Alpha, Beta, _, _, Alpha, Beta).

evaluate(m(F1, F2, F3, F4), Score) :-
    % Flatten the board to make it easier to work with
    F1 = f(A1, B1, C1, D1), F2 = f(A2, B2, C2, D2), F3 = f(A3, B3, C3, D3), F4 = f(A4, B4, C4, D4),
    append([A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3, A4, B4, C4, D4], Board),
    % Compute the various heuristic scores based on the board
    count_empty(Board, EmptyCellsScore),
    sum_tiles(Board, TileValuesScore),
    max_tile(Board, MaxTileScore),
    calculate_monotonicity(Board, MonotonicityScore),
    % Combine these scores using some formula to compute a final score for the board
    Score is EmptyCellsScore * 256 + TileValuesScore + MaxTileScore * 16 + MonotonicityScore.

count_empty(Board, EmptyCellsScore) :-
    include(=(('-')), Board, EmptyCells),
    length(EmptyCells, EmptyCellsScore).

% sum_tiles/2 predicate
sum_tiles(Board, TileValuesScore) :-
    exclude(=('-'), Board, Tiles),
    sum_list(Tiles, TileValuesScore).

% max_tile/2 predicate
max_tile(Board, MaxTileScore) :-
    exclude(=('-'), Board, Tiles),
    max_list(Tiles, MaxTileScore).

% calculate_monotonicity/2 predicate
calculate_monotonicity(m(F1, F2, F3, F4), MonotonicityScore) :-
    calculate_row_monotonicity([F1, F2, F3, F4], RowScore),
    transpose([F1, F2, F3, F4], [C1, C2, C3, C4]),
    calculate_row_monotonicity([C1, C2, C3, C4], ColScore),
    MonotonicityScore is RowScore + ColScore.

% calculate_row_monotonicity/2 predicate
calculate_row_monotonicity([], 0).
calculate_row_monotonicity([Row|Rows], Score) :-
    calculate_row_monotonicity(Rows, ScoreRest),
    monotonicity(Row, RowScore),
    Score is ScoreRest + RowScore.

% monotonicity/2 predicate
monotonicity([], 0).
monotonicity([_], 0).
monotonicity([X, Y|T], Score) :-
    monotonicity([Y|T], ScoreRest),
    ( X =< Y
    -> Score is ScoreRest + 1
    ; Score is ScoreRest - 1
    ).

% Board representation.
m(f(_, _, _, _),f(_, _, _, _),f(_, _, _, _),f(_, _, _, _)).

%%%%%%%%%%%%%%%%%%%%%%%%% FIN mejor_movimiento %%%%%%%%%%%%%%%%%%%%%%%%%

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

