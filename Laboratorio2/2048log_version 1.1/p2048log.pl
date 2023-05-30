% list of possible moves
moves([up, down, left, right]).

% random strategy
strategy(random, Board, _, Move) :-
	findall(Move1, (member(Move1, [up, down, left, right]), movimientoT(Board, Move1, NewBoard, _), Board \== NewBoard), Moves),
    (Moves == [] -> Move = up ; random_member(Move, Moves)).

% dummy strategy
strategy(dummy, Board, _, Move) :-
    moves(Moves),
    findall(Score-Mv, (member(Mv, Moves), movimientoT(Board, Mv, NewBoard, Score), Board \== NewBoard), ScoresMoves),
    max_member(_-Move, ScoresMoves).

% ai strategy
strategy(ai, Board, Depth, Move) :-
    findall(Score-Move1, (member(Move1, [up, down, left, right]), calculateScore(Board, Depth, Move1, Score)), ScoresMoves),
    max_member(_-Move, ScoresMoves).

% calculateScore(+Board, +Depth, +Move, -Score)
calculateScore(Board, Depth, Move, Score) :-
	movimientoT(Board, Move, NewBoard, _),
	Board \== NewBoard,
	generateScore(NewBoard, Depth, Score,0),
	delete_all_cached_values.

% Declare the predicate as dynamic
:- dynamic cached_value/2.

% Define a predicate to retrieve the cached value
set_cached_value(Key, Value) :-
    % If the value is not cached, compute it and cache it
    assertz(cached_value(Key, Value)).

% Define a predicate to delete a cached value
delete_cached_value(Key) :-
    % Remove the cached value
    retract(cached_value(Key, _)).

% Define a predicate to delete all cached values
delete_all_cached_values :-
    % Find all the keys of the cached values
    findall(Key, cached_value(Key, _), Keys),
    % Delete each cached value
    maplist(delete_cached_value, Keys).

% Define a predicate to check if a value is already cached
is_value_cached(Key, Value) :-
    % Check if the value is already cached
    cached_value(Key, Value), !.

% generateScore(+Board, +Depth, -Score)
generateScore(Board, 0, Score,_) :-
	evaluate_board(Board, Score).
generateScore(Board, Depth, Score, CleanCache) :-
    (CleanCache ==  2 ->  delete_all_cached_values ; true),
	Depth > 0,
	findall(
		CellScore, 
		(
			arg(Row, Board, RowFunctor), 
			arg(Col, RowFunctor, Value), 
			Value == '-',
			(is_value_cached(Row-Col, CachedCellScore) -> 
				CellScore = CachedCellScore ;
				simulate2(Board, Depth, Row, Col, Score2, CleanCache),
				simulate4(Board, Depth, Row, Col, Score4, CleanCache),
				CellScore is Score2 + Score4,
				set_cached_value(Row-Col, CellScore))
		), 
		CellsScores
	),
	sum_list(CellsScores, Score).

% multiply/3 multiplies two numbers and returns a floating-point result
multiply(X, Y, Result) :-
    Result is float(X) * float(Y).

% simulate2(+Board, +Depth, +Row, +Col, -Score) -> Simulate the score if the cell is filled with 2 (2 has a 80% chance of happening)
simulate2(Board, Depth, Row, Col, Score2, CleanCache) :-
	arg(Row, Board, RowFunctor),
	setarg(Col, RowFunctor, 2),
	setarg(Row, Board, RowFunctor),
	calculateMoveScore(Board, Depth, MoveScore, CleanCache),
	multiply(MoveScore, 0.8, Score2).

% simulate4(+Board, +Depth, +Row, +Col, -Score) -> Simulate the score if the cell is filled with 4 (4 has a 20% chance of happening)
simulate4(Board, Depth, Row, Col, Score4, CleanCache) :-
	arg(Row, Board, RowFunctor),
	setarg(Col, RowFunctor, 4),
	setarg(Row, Board, RowFunctor),
	calculateMoveScore(Board, Depth, MoveScore, CleanCache),
	multiply(MoveScore, 0.2, Score4).

% calculateMoveScore(+Board, +Depth, -Score) -> Calculate the score of the move
calculateMoveScore(Board, Depth, Score, CleanCache) :-
    (CleanCache == 2 ->  NewLevelCache = 0; NewLevelCache is CleanCache + 1),
	NextDepth is Depth - 1,
	findall(
		MoveScore, 
		(
			member(Move, [up, down, left, right]), 
			movimientoT(Board, Move, NewBoard, _), 
			Board \== NewBoard, 
			generateScore(NewBoard, NextDepth, MoveScore, NewLevelCache)
		), 
		MovesScores
	),
	max_member(Score, MovesScores).

% edge_score(+Board, -Score) -> This heuristic gives higher scores to tiles placed on the edges of the board.
edge_score(Board, Score) :-
    functor(Board, _, Rows),
    functor(Board, _, Cols),
    findall(Value, (arg(Row, Board, RowFunctor), (Row =:= 1; Row =:= Rows), arg(Col, RowFunctor, Value), (Col =:= 1; Col =:= Cols), (Value == '-' -> Value is 0; true)), EdgeValues),
    sum_list(EdgeValues, Score).

% monotonicity(+Board, -Score) -> This heuristic measures the degree to which the values of the tiles are ordered in a specific direction (e.g., increasing or decreasing)
monotonicity(Board, Score) :-
    findall(RowScore, (arg(_, Board, RowFunctor), row_monotonicity(RowFunctor, RowScore)), RowScores),
    sum_list(RowScores, Score).

% row_monotonicity(+RowFunctor, -Score)
row_monotonicity(RowFunctor, Score) :-
    functor(RowFunctor, _, Cols),
    row_monotonicity(Cols, RowFunctor, Score).

% row_monotonicity(+Cols, +RowFunctor, -Score)
row_monotonicity(1, _, 0).
row_monotonicity(Cols, RowFunctor, Score) :-
    Cols > 1,
    arg(Cols, RowFunctor, X),
	( X == '-' -> X is 0; true),
	Cols1 is Cols - 1,
    arg(Cols1, RowFunctor, Y),
	( Y == '-' -> Y is 0; true),
    row_monotonicity(Cols1, RowFunctor, Score1),
    (X =< Y -> Score is Score1 + 1; Score is Score1).

% smoothness(+Board, -Score) :- This heuristic measures the difference in values between adjacent tiles.
smoothness(Board, Score) :-
    findall(RowScore, (arg(_, Board, RowFunctor), row_smoothness(RowFunctor, RowScore)), RowScores),
    sum_list(RowScores, Score).

% row_smoothness(+RowFunctor, -Score)
row_smoothness(RowFunctor, Score) :-
    functor(RowFunctor, _, Cols),
    row_smoothness(Cols, RowFunctor, Score).

% row_smoothness(+Cols, +RowFunctor, -Score)
row_smoothness(1, _, 0).
row_smoothness(Cols, RowFunctor, Score) :-
    Cols > 1,
    arg(Cols, RowFunctor, X),
	(X == '-' -> X is 0; true),
	Cols1 is Cols - 1,
    arg(Cols1, RowFunctor, Y),
	(Y == '-' -> Y is 0; true),
    row_smoothness(Cols1, RowFunctor, Score1),
    Diff is abs(X - Y),
    Score is Score1 + Diff.

% max_tile(+Board, -MaxTileScore) -> This heuristic gives higher scores to boards with higher tiles.
max_tile(Board, MaxTileScore) :-
	findall(Value, (arg(_, Board, RowFunctor), arg(_, RowFunctor, Value), (Value == '-' -> Value is 0; true)), Values),
	max_list(Values, MaxTileScore).

% count_empty_cells(+Board, -Count) -> This heuristic count the number of empty cells.
count_empty_cells(Board, Count) :-
    findall(1, (arg(_, Board, RowFunctor), arg(_, RowFunctor, Cell), Cell == '-'), Empties),
    length(Empties, Count).

% board evaluation function
evaluate_board(Board, Score) :-
	monotonicity(Board, Monotonicity),
	smoothness(Board, Smoothness),
	count_empty_cells(Board, EmptyCells),
	max_tile(Board, MaxTile),
	Score is (EmptyCells * 256) + (Monotonicity * 128) + (MaxTile * 64) - (Smoothness * 32).

% main predicate for choosing the best move
mejor_movimiento(Tablero, NivelMiniMax, Estrategia, Jugada) :-
    strategy(Estrategia, Tablero, NivelMiniMax, Jugada),
    !.
mejor_movimiento(_, _, _, up).

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
testPlayGame(Board, NivelMiniMax, Strategy, Won, CurrentScore, FinalScore, FinalBoard) :-
    (game_over(Board) -> 
		Won = false,
		FinalScore = CurrentScore,
		FinalBoard = Board;
    	mejor_movimiento(Board, NivelMiniMax, Strategy, Move),
		movimientoT(Board, Move, NewBoard, Score),
		CurrentScore1 is CurrentScore + Score,
    	(testGameWon(NewBoard) -> 
			Won = true,
			FinalScore = CurrentScore1,
			FinalBoard = NewBoard;
			add_next_number(NewBoard, FullNewBoard),
        	testPlayGame(FullNewBoard, NivelMiniMax, Strategy, Won, CurrentScore1, FinalScore, FinalBoard)
		)
	).

% Add a new number to the board
add_next_number(Grid, NewGrid) :-
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
			  f(M, N, O, P))) :- member(2048, [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]).

% Dummy predicate to check if the game is over (no more moves can be made)
game_over(Board) :-
	movimientoT(Board, up, NewBoard, _),
	Board == NewBoard,
	movimientoT(Board, down, NewBoard1, _),
	Board == NewBoard1,
	movimientoT(Board, left, NewBoard2, _),
	Board == NewBoard2,
	movimientoT(Board, right, NewBoard3, _),
	Board == NewBoard3.

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
	test(1, Stream, 4),
    close(Stream).

% test/1 generate 10 initial boards and plays 100 games for each board
test(11, _, _).
test(Counter, Stream, NivelMiniMax) :-
	initial_board(Board),
	write(Stream,'Testing board: '),
	write(Stream, Counter),
	write(Stream, ' '),
	write(Stream, Board),
	nl(Stream),
	testLoop(100, Stream, Board, NivelMiniMax, 'ai'),
	Counter1 is Counter + 1,
	test(Counter1, Stream, NivelMiniMax).

% testLoop play 100 games with the same initial board and writes the results to a file
testLoop(101, _, _, _, _).
testLoop(Counter, Stream, Board, NivelMiniMax, Strategy) :-
    testPlayGame(Board, NivelMiniMax, Strategy, Won, 0, FinalScore, FinalBoard),
    write(Stream, Counter),
    write(Stream, ' '),
	(Won -> write(Stream, 'Won') ; write(Stream, 'Lost')),
	write(Stream, ' '),
	write(Stream, FinalScore),
	nl(Stream),
	% Write the final board row by row
	arg(1, FinalBoard, Row1),
	arg(2, FinalBoard, Row2),
	arg(3, FinalBoard, Row3),
	arg(4, FinalBoard, Row4),
	write(Stream, Row1),
	nl(Stream),
	write(Stream, Row2),
	nl(Stream),
	write(Stream, Row3),
	nl(Stream),
	write(Stream, Row4),
	nl(Stream),
    Counter1 is Counter + 1,
    testLoop(Counter1, Stream, Board, NivelMiniMax, Strategy).

% empty_board(m(f('-', '-', '-', '-'),f('-', '-', '-', '-'),f('-', '-', '-', '-'),f('-', '-', '-', '-'))).
% testGameWon(m(f(2048, '-', '-', '-'),f('-', '-', '-', '-'),f('-', '-', '-', '-'),f('-', '-', '-', '-'))).