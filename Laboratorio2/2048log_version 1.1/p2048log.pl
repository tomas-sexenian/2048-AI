% lista de movimientos posibles
moves([up, down, left, right]).


% strategy(+NombreEstrategia, +Tablero, ?NivelMinimax, -Jugada) -> strategy toma la estrategia a utilizar, el tablero y 
% el nivel minimax (en caso de que la estrategia sea ia), devuelve la jugada a realizar de acuerdo a la estrategia

% estrategia aleatoria
strategy(random, Board, _, Move) :-
	findall(Move1, (member(Move1, [up, down, left, right]), movimientoT(Board, Move1, NewBoard, _), Board \== NewBoard), Moves),
    (Moves == [] -> Move = up ; random_member(Move, Moves)).

% estrategia dummy
strategy(dummy, Board, _, Move) :-
    moves(Moves),
    findall(Score-Mv, (member(Mv, Moves), movimientoT(Board, Mv, NewBoard, Score), Board \== NewBoard), ScoresMoves),
    max_member(_-Move, ScoresMoves).

% estrategia ia
strategy(ia, Board, Depth, Move) :-
    findall(Score-Move1, (member(Move1, [up, down, left, right]), calculateScore(Board, Depth, Move1, Score)), ScoresMoves),
    max_member(_-Move, ScoresMoves).

% calculateScore(+Board, +Depth, +Move, -Score) -> calculateScore toma el estado actual del tablero, la profundidad minimax y 
% el siguiente movimiento a realizar, devuelve el puntaje luego de realizar el movimiento
calculateScore(Board, Depth, Move, Score) :-
	movimientoT(Board, Move, NewBoard, _),
	Board \== NewBoard,
	generateScore(NewBoard, Depth, Score,0),
	delete_all_cached_values.

% Declarar el predicado como dinámico
:- dynamic cached_value/2.

% set_cached_value(+Key, +Value) -> Dado un (clave, valor) agrega una nueva regla cached_value(clave, valor).
set_cached_value(Key, Value) :-
    assertz(cached_value(Key, Value)).

% delete_cached_value(+Key) -> Dado una clave elimina todas las reglas cached_value(clave, valor) de clave = key.
delete_cached_value(Key) :-
    retract(cached_value(Key, _)).

% delete_all_cached_values -> Borra todas las reglas cuyo funtor sea cached_value
delete_all_cached_values :-
    findall(Key, cached_value(Key, _), Keys),
    maplist(delete_cached_value, Keys).

% is_value_cached(+Key, +Value) -> Verifica si la regla cached_value(Key, Value) pertenece al programa logico
is_value_cached(Key, Value) :-
    cached_value(Key, Value), !.

% generateScore(+Board, +Depth, -Score, +CleanCache) -> generateScore toma el estado actual del tablero, la profundidad minimax, cuando
% CleanCache es igual a 2 se borran los datos cacheados hasta el momento. Devuelve el puntaje que el tablero tendra luego de Depth movimientos
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

% multiply(+X, +Y, -Result) multiplica dos números y devuelve un resultado de punto flotante
multiply(X, Y, Result) :-
    Result is float(X) * float(Y).

% simulate2(+Board, +Depth, +Row, +Col, -Score) -> Simula el puntaje si la celda (Row, Col) se llena con 2 (2 tiene un 80% de posibilidades de suceder)
simulate2(Board, Depth, Row, Col, Score2, CleanCache) :-
	arg(Row, Board, RowFunctor),
	setarg(Col, RowFunctor, 2),
	setarg(Row, Board, RowFunctor),
	calculateMoveScore(Board, Depth, MoveScore, CleanCache),
	multiply(MoveScore, 0.8, Score2).

% simulate4(+Board, +Depth, +Row, +Col, -Score) -> Simula el puntaje si la celda (Row, Col) se llena con 4 (4 tiene un 20% de probabilidad de suceder)
simulate4(Board, Depth, Row, Col, Score4, CleanCache) :-
	arg(Row, Board, RowFunctor),
	setarg(Col, RowFunctor, 4),
	setarg(Row, Board, RowFunctor),
	calculateMoveScore(Board, Depth, MoveScore, CleanCache),
	multiply(MoveScore, 0.2, Score4).

% calculateMoveScore(+Board, +Depth, -Score, +CleanCache) -> Calcula la puntuación de la jugada
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

% monotonicity(+Board, -Score) -> Esta heurística mide el grado en que los valores del tablero están ordenados en una dirección específica (aumentando o disminuyendo)
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

% smoothness(+Board, -Score) -> Esta heurística mide la diferencia de valores entre las celdas adyacentes.
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

% max_tile(+Board, -MaxTileScore) -> Esta heurística devuelve el mayor valor entre los valores de todas las celdas.
max_tile(Board, MaxTileScore) :-
	findall(Value, (arg(_, Board, RowFunctor), arg(_, RowFunctor, Value), (Value == '-' -> Value is 0; true)), Values),
	max_list(Values, MaxTileScore).

% count_empty_cells(+Board, -Count) -> Esta heurística devuelve la cantidad de celdas vacias del tablero.
count_empty_cells(Board, Count) :-
    findall(1, (arg(_, Board, RowFunctor), arg(_, RowFunctor, Cell), Cell == '-'), Empties),
    length(Empties, Count).

% evaluate_board(+Board, -Score) -> Toma el tablero y devuelve el puntaje asociado segun las heuristicas
evaluate_board(Board, Score) :-
	monotonicity(Board, Monotonicity),
	smoothness(Board, Smoothness),
	count_empty_cells(Board, EmptyCells),
	max_tile(Board, MaxTile),
	Score is (EmptyCells * 256) + (Monotonicity * 128) + (MaxTile * 64) - (Smoothness * 32).

% mejor_movimiento(+Tablero,+NivelMiniMax,+Estrategia,-Jugada) -> +Tablero representa un tablero con el estado del juego, +NivelMiniMax 
% representa el nivel para el algoritmo miminax, +Estrategia representa el nombre de la estrategia a utilizar (este argumento permite probar mas de una estrategia), 
% y -Jugada es un valor de retorno que representa la dirección del movimiento (arriba, abajo, izquierda, derecha)
mejor_movimiento(Tablero, NivelMiniMax, Estrategia, Jugada) :-
    strategy(Estrategia, Tablero, NivelMiniMax, Jugada),
    !.
mejor_movimiento(_, _, _, up).

% Inicio de predicado movimientoT

% movimientoT(+Board,+Direction,-BoardNew,-ScoreGen) -> Este predicado recibe un tablero en el predicado llamado m de formato de aridad 4,
% y una dirección de movimiento, e implementa el movimiento del tablero en la dirección indicada. BoardNew y ScoreGen devuelven el nuevo 
% tablero y la puntuación generada durante el movimiento
movimientoT(Tablero, Movimiento, TableroNew, ScoreGen) :-
    once(aux_left(Tablero, Movimiento, TableroNew, ScoreGen));
	once(aux_up(Tablero, Movimiento, TableroNew, ScoreGen));
	once(aux_right(Tablero, Movimiento, TableroNew, ScoreGen));
	once(aux_down(Tablero, Movimiento, TableroNew, ScoreGen)).

% aux_up(+Tablero, up, -TableroNew, -ScoreGen) -> recibe el estado actual del tablero y el movimiento hacia arriba,
% devuelve el tablero resultado de mover hacia arriba y el puntaje generado.
aux_up(Tablero, up, TableroNew, ScoreGen) :-
    tolist(Tablero, Tnew),
    moverArriba(Tnew, Tsol, ScoreGen),
    reconstruir_tablero(Tsol,TableroNew).

% aux_left(+Tablero, left, -TableroNew, -ScoreGen) -> recibe el estado actual del tablero y el movimiento hacia la izquierda,
% devuelve el tablero resultado de mover hacia la izquierda y el puntaje generado.
aux_left(Tablero, left, TableroNew, ScoreGen) :-
    tolist(Tablero, Tnew),
    moverizquierda(Tnew, Tsol, ScoreGen),
    reconstruir_tablero(Tsol,TableroNew).

% aux_right(+Tablero, right, -TableroNew, -ScoreGen) -> recibe el estado actual del tablero y el movimiento hacia la derecha,
% devuelve el tablero resultado de mover hacia la derecha y el puntaje generado.
aux_right(Tablero, right, TableroNew, ScoreGen) :-
    tolist(Tablero, Tnew),
    moverderecha(Tnew, Tsol, ScoreGen),
    reconstruir_tablero(Tsol,TableroNew).

% aux_down(+Tablero, down, -TableroNew, -ScoreGen) -> recibe el estado actual del tablero y el movimiento hacia abajo,
% devuelve el tablero resultado de mover hacia abajo y el puntaje generado.
aux_down(Tablero, down, TableroNew, ScoreGen) :-
    tolist(Tablero, Tnew),
    moverabajo(Tnew, Tsol, ScoreGen),
    reconstruir_tablero(Tsol,TableroNew).


% reconstruir_tablero(+[A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], -Tablero) -> recibe la representacion en lista del
% tablero y su representacion en el predicado m(f(...),f(...),f(...),f(...))
reconstruir_tablero([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], 
                  Tablero):-
    TermA =.. [f, A1, A2, A3, A4],
    TermB =.. [f, B1, B2, B3, B4],
    TermC =.. [f, C1, C2, C3, C4],
    TermD =.. [f, D1, D2, D3, D4],
    Tablero =.. [m, TermA, TermB, TermC, TermD].

% tolist(+Tablero, -Lista) -> recibe el tablero en su representacion en el predicado m(f(...),f(...),f(...),f(...)) y devuelve su representacion
% como lista 
tolist(Tablero, Lista) :-
    Tablero =.. [ _, A1, A2, A3, A4],
    A1 =.. [_ | Args1],
    A2 =.. [_ | Args2],
    A3 =.. [_ | Args3],
    A4 =.. [_ | Args4],
    append(Args1, Args2, Args12),
    append(Args12, Args3, Args123),
    append(Args123, Args4, Lista).
    
% rotartableroder(+Tablero, -TableroRotado) -> recibe el tablero representado como una lista y lo rota hacia la derecha
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

% rotartableroizq(+Tablero, -TableroRotado) -> recibe el tablero representado como una lista y lo rota hacia la izquierda
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

% moverArriba(+Tablero, -TableroNew, -ScoreGen) -> recibe el tablero representado como una lista y devuelve el tablero resultante de 
% mover hacia arriba y el puntaje generado
moverArriba(Tablero, TableroNew, ScoreGen):-
	rotartableroizq(Tablero, Tnew1),
	moverizquierda(Tnew1, Tnew2, ScoreGen),
	rotartableroder(Tnew2, TableroNew).

% moverabajo(+Tablero, -TableroNew, -ScoreGen) -> recibe el tablero representado como una lista y devuelve el tablero resultante de 
% mover hacia abajo y el puntaje generado
moverabajo(Tablero, TableroNew, ScoreGen):-
	rotartableroder(Tablero, Tnew1),
	moverizquierda(Tnew1, Tnew2, ScoreGen),
	rotartableroizq(Tnew2, TableroNew).

% moverderecha(+Tablero, -TableroNew, -ScoreGen) -> recibe el tablero representado como una lista y devuelve el tablero resultante de 
% mover hacia la derecha y el puntaje generado
moverderecha(Tablero, TableroNew, ScoreGen):-
	rotartableroizq(Tablero, Tnew1),
	rotartableroizq(Tnew1, Tnew2),
	moverizquierda(Tnew2, Tnew3, ScoreGen),
	rotartableroder(Tnew3, Tnew4),
	rotartableroder(Tnew4, TableroNew).

% moverizquierda(+Tablero, -TableroNew, -ScoreGen) -> recibe el tablero representado como una lista y devuelve el tablero resultante de 
% mover hacia la izquierda y el puntaje generado
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

% testPlayGame juega un juego usando mejor_movimiento y devuelve el puntaje final y una bandera que indica si el juego fue ganado
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

% Añade un nuevo número al tablero
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

% Predicado dummy para comprobar si se ha ganado el juego (se ha alcanzado una ficha 2048)
testGameWon(m(f(A, B, C, D),
			  f(E, F, G, H),
			  f(I, J, K, L),
			  f(M, N, O, P))) :- member(2048, [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]).

% Predicado dummy para verificar si el juego termino (no se pueden realizar mas movimientos)
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

% test/0 genera datos de prueba para el juego
test :- 
	open('2048_test_data.txt', write, Stream),
	test(1, Stream, 4),
    close(Stream).

% test/3 genera 10 tableros iniciales y juega 100 juegos para cada tablero
test(11, _, _).
test(Counter, Stream, NivelMiniMax) :-
	initial_board(Board),
	nl(Stream),
	testLoop(1, Stream, Board, NivelMiniMax, 'ia'),
	Counter1 is Counter + 1,
	test(Counter1, Stream, NivelMiniMax).

% testLoop juega 100 juegos con el mismo tablero inicial y escribe los resultados en un archivo
testLoop(34, _, _, _, _).
testLoop(Counter, Stream, Board, NivelMiniMax, Strategy) :-
    testPlayGame(Board, NivelMiniMax, Strategy, Won, 0, FinalScore, FinalBoard),
    write(Stream, Counter),
    write(Stream, ' '),
	(Won -> write(Stream, 'Won') ; write(Stream, 'Lost')),
	write(Stream, ' '),
	write(Stream, FinalScore),
	nl(Stream),
    Counter1 is Counter + 1,
    testLoop(Counter1, Stream, Board, NivelMiniMax, Strategy).