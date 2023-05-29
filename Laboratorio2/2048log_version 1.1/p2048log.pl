mejor_movimiento(Tablero, _, Estrategia, Jugada) :-
    Estrategia = random, random_move(Tablero, Jugada), !.
mejor_movimiento(Tablero, _, Estrategia, Jugada) :-
    Estrategia = dummy, dummy_move(Tablero, Jugada), !.
mejor_movimiento(Tablero, _, Estrategia, Jugada) :-
	writeln('acaaaaaaaaaa'),
	writeln(Tablero),
    Estrategia = ia,
    ia_move(Tablero, 4, Jugada).

random_move(Tablero, Jugada) :-
	writeln('random_move'),
    findall(Jugada, (member(Jugada, [up, down, left, right]), movimientoT(Tablero, Jugada, TableroNew, _), \+ Tablero = TableroNew), Jugadas),
    (Jugadas = [] -> Jugada = up ; random_member(Jugada, Jugadas)).

dummy_move(Tablero, Jugada) :-
	writeln('dummy_move'),
    findall(Score-Jugada, (member(Jugada, [up, down, left, right]), movimientoT(Tablero, Jugada, TableroNew, Score), \+ Tablero = TableroNew), ScoreList),
    (ScoreList = [] -> Jugada = up ; (keysort(ScoreList, SortedScoreList), reverse(SortedScoreList, [_-Jugada|_]))).

ia_move(Tablero, NivelMiniMax, Jugada) :-
	writeln('ia_move'),
	writeln(NivelMiniMax),
    tolist(Tablero, ListTablero),
	once(moverizquierda(ListTablero, LeftTablero, _)),
    (equal(ListTablero, LeftTablero) -> ScoreL = 0 ;  evaluate(ListTablero, LeftTablero, NivelMiniMax, ScoreL)),
	once(moverderecha(ListTablero, RightTablero, _)),
    (equal(ListTablero, RightTablero) -> ScoreR = 0 ;  evaluate(ListTablero, RightTablero, NivelMiniMax, ScoreR)),
	once(moverArriba(ListTablero, UpTablero, _)),
    (equal(ListTablero, UpTablero) -> ScoreU = 0 ;  evaluate(ListTablero, UpTablero, NivelMiniMax, ScoreU)),
	once(moverabajo(ListTablero, DownTablero, _)),
    (equal(ListTablero, DownTablero) -> ScoreD = 0 ;  evaluate(ListTablero, DownTablero, NivelMiniMax, ScoreD)),
	selectMove(ScoreL, ScoreR, ScoreU, ScoreD, Jugada),
    writeln(ScoreL),writeln(ScoreR),writeln(ScoreU),writeln(ScoreD),
	writeln(Jugada).

% checks if two lists are equal
equal([],[]).
equal([H1|T1],[H2|T2]) :-
	H1 == H2,
	equal(T1,T2).

% if the move is not possible, it gets a score of 0
evaluate(Board, NewBoard, _, 0) :-
	equal(Board, NewBoard).
% this, along with the evalNext and evalMoves predicates search the game
% space for the best move
evaluate(_, Board, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	boardScore(Board, MejorScoreActual),
    findall(Score, (between(0, 15, Number), evalNext(Board,Number,NewLevel, Score)), Scores),
    sum_list(Scores, SumScore),
	Score is 10*MejorScoreActual+SumScore. 

evalNext(['-',A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 0, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([2,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [4,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,'-',A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 1, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],[A1,4,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], NewLevel,Score).
evalNext([A1,A2,'-',A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 2, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,2,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [A1,A2,4,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,'-',B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 3, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,2,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [A1,A2,A3,4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,'-',B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 4, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,2,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [A1,A2,A3,A4,4,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,'-',B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 5, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [A1,A2,A3,A4,B1,4,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,'-',B4,C1,C2,C3,C4,D1,D2,D3,D4], 6, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,2,B4,C1,C2,C3,C4,D1,D2,D3,D4], [A1,A2,A3,A4,B1,B2,4,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,'-',C1,C2,C3,C4,D1,D2,D3,D4], 7, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,2,C1,C2,C3,C4,D1,D2,D3,D4], [A1,A2,A3,A4,B1,B2,B3,4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,B4,'-',C2,C3,C4,D1,D2,D3,D4], 8, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,B4,2,C2,C3,C4,D1,D2,D3,D4], [A1,A2,A3,A4,B1,B2,B3,B4,4,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,B4,C1,'-',C3,C4,D1,D2,D3,D4], 9, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,2,C3,C4,D1,D2,D3,D4], [A1,A2,A3,A4,B1,B2,B3,B4,C1,4,C3,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,'-',C4,D1,D2,D3,D4], 10, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,2,C4,D1,D2,D3,D4], [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,4,C4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,'-',D1,D2,D3,D4], 11, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,2,D1,D2,D3,D4], [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,4,D1,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,'-',D2,D3,D4], 12, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,2,D2,D3,D4], [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,4,D2,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,'-',D3,D4], 13, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,2,D3,D4],[A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,4,D3,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,'-',D4], 14, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,2,D4], [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,4,D4],NewLevel,Score).
evalNext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,'-'], 15, Level, Score) :-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,2], [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,4],NewLevel,Score).
evalNext(_, _, _, 0).

evalMoves(B2,B4, Level, Score) :-
	once(moverizquierda(B2, B2L, _)),
    (equal(B2, B2L) -> S2L = 0 ;  evaluate(B2, B2L, Level, S2L)),
    once(moverderecha(B2, B2R, _)),
    (   equal(B2, B2R) -> S2R = 0 ;  evaluate(B2, B2R, Level, S2R)),
    once(moverArriba(B2, B2U, _)),
    (   equal(B2, B2U) -> S2U = 0 ;  evaluate(B2, B2U, Level, S2U)),
    once(moverabajo(B2, B2D, _)),
    (   equal(B2, B2D) -> S2D = 0 ;  evaluate(B2, B2D, Level, S2D)),
    once(moverizquierda(B4, B4L, _)),
    (   equal(B4, B4L) -> S4L = 0 ;  evaluate(B4, B4L, Level, S4L)),
    once(moverderecha(B4, B4R, _)),
    (   equal(B4, B4R) -> S4R = 0 ;  evaluate(B4, B4R, Level, S4R)),
    once(moverArriba(B4, B4U, _)),
   (    equal(B4, B4U) -> S4U = 0 ;  evaluate(B4, B4U, Level, S4U)),
    once(moverabajo(B4, B4D, _)),
    (   equal(B4, B4D) -> S4D = 0 ;  evaluate(B4, B4D, Level, S4D)),
	Score is 9*(S2L+S2R+S2U+S2D)+S4L+S4R+S4U+S4D.


boardScore(Board, Score) :-
    squared(Board, Squared),
	sum_list(Squared, Score).

% square every value of a list
squared([], []).
squared([H1|T1], T2) :-
	H1 == -,
	squared(T1, T2).
squared([H1|T1], [H2|T2]) :-
	H1 \= -,
	H2 is H1 * H1,
	squared(T1,T2).
% the move with the highest score is selected
selectMove(ScoreL, ScoreR, ScoreU, ScoreD, right) :-
	ScoreR >= ScoreU,
	ScoreR >= ScoreD,
	ScoreR >= ScoreL.
selectMove(ScoreL, ScoreR, ScoreU, ScoreD, up) :-
	ScoreU >= ScoreR,
	ScoreU >= ScoreD,
	ScoreU >= ScoreL.
selectMove(ScoreL, ScoreR, ScoreU, ScoreD, down) :-
	ScoreD >= ScoreU,
	ScoreD >= ScoreR,
	ScoreD >= ScoreL.
selectMove(_, _, _, _, left).

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
    	mejor_movimiento(Board, 4, Strategy, Move),
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
test(2, _).
test(Counter, Stream) :-
	initial_board(Board),
	write(Stream,'Testing board: '),
	write(Stream, Counter),
	write(Stream, ' '),
	write(Stream, Board),
	nl(Stream),
	testLoop(1, Stream, Board, ia),
	Counter1 is Counter + 1,
	test(Counter1, Stream).

% testLoop play 100 games with the same initial board and writes the results to a file
testLoop(3, _, _, _).
testLoop(Counter, Stream, Board, Strategy) :-
    testPlayGame(Board, Won, Strategy),
    write(Stream, Counter),
    write(Stream, ' '),
	(Won -> write(Stream, 'Won') ; write(Stream, 'Lost')),
    nl(Stream),
    Counter1 is Counter + 1,
    testLoop(Counter1, Stream, Board, Strategy).