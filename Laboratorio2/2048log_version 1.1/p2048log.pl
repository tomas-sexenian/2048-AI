
% movimientoT(+Tablero,+Direccion,-TableroNew,-ScoreGen): toma el tablero y mueve las piezas hacia la dirección correspondiente
movimientoT(Tablero,Dir,TableroNew,ScoreGen):-

% mejor_movimiento(+Tablero,+NivelMiniMax,+Estrategia,-Jugada): devuelve la mejor jugada posible, a partir de un tablero y una estrategia

% mejor_movimiento(Tablero,Nivel_minimax,Estrategia,Jugada):-

% As of below, everything is GPT-4 generated

% movementT(+Board, +Direction, -BoardNew, -ScoreGen)
movementT(Board, Direction, BoardNew, ScoreGen) :-
    rotate_board(Direction, Board, RotatedBoard),
    move_rows(RotatedBoard, MovedBoard, ScoreGen),
    rotate_board_back(Direction, MovedBoard, BoardNew).

% Helper predicates to rotate the board depending on the direction
rotate_board(up, Board, Board).
rotate_board(down, Board, RotatedBoard) :- rotate_board_180(Board, RotatedBoard).
rotate_board(left, Board, RotatedBoard) :- transpose(Board, RotatedBoard).
rotate_board(right, Board, RotatedBoard) :- transpose(Board, Transposed), rotate_board_180(Transposed, RotatedBoard).

rotate_board_back(up, Board, Board).
rotate_board_back(down, Board, RotatedBoard) :- rotate_board_180(Board, RotatedBoard).
rotate_board_back(left, Board, RotatedBoard) :- transpose(Board, RotatedBoard).
rotate_board_back(right, Board, RotatedBoard) :- rotate_board_180(Board, Rotated), transpose(Rotated, RotatedBoard).

% Helper predicates to rotate the board 180 degrees
rotate_board_180(Board, RotatedBoard) :-
    reverse(Board, Reversed),
    maplist(reverse, Reversed, RotatedBoard).

% Helper predicate to move rows to the left and combine tiles
move_rows(Board, MovedBoard, ScoreGen) :-
    maplist(move_row, Board, MovedBoard, Scores),
    sum_list(Scores, ScoreGen).

move_row(Row, NewRow, Score) :-
    compress(Row, Compressed, 0, Score),
    length(Compressed, Len),
    NewRowSize is 4 - Len,
    length(NewRow, 4),
    append(Compressed, [- | Zeros], NewRow),
    length(Zeros, NewRowSize).

% Helper predicate to combine and move tiles to the left
compress([], [], Score, Score).
compress([X, X | T], [X2 | R], ScoreAcc, Score) :-
    !,
    X2 is X * 2,
    NewScoreAcc is ScoreAcc + X2,
    compress(T, R, NewScoreAcc, Score).
compress([- | T], R, ScoreAcc, Score) :-
    !,
    compress(T, R, ScoreAcc, Score).
compress([H | T], [H | R], ScoreAcc, Score) :-
    compress(T, R, ScoreAcc, Score).

% mejor_movimiento(+Tablero, +NivelMiniMax, +Estrategia, -Jugada)
mejor_movimiento(Board, _, random, Move) :-
    random_move(Board, Move).

mejor_movimiento(Board, _, dummy, Move) :-
    dummy_move(Board, Move).

mejor_movimiento(Board, NivelMiniMax, ia, Move) :-
    ia_move(Board, NivelMiniMax, Move).

% Implementing the random strategy
random_move(Board, Move) :-
    findall(M, valid_move(Board, M), ValidMoves),
    length(ValidMoves, Len),
    Len > 0,
    random(0, Len, Index),
    nth0(Index, ValidMoves, Move).

% Implementing the dummy strategy
dummy_move(Board, Move) :-
    findall((M, S), (valid_move(Board, M), movementT(Board, M, _, S)), ValidMovesScores),
    keysort(ValidMovesScores, SortedMovesScores),
    reverse(SortedMovesScores, DescendingMovesScores),
    DescendingMovesScores = [(Move, _Score) | _Rest].

% Implementing the ia strategy
ia_move(Board, NivelMiniMax, Move) :-
    % Implement your custom IA strategy here using the NivelMiniMax parameter
    % As an example, we will use the dummy strategy for now
    dummy_move(Board, Move).

% Helper predicate to check if a move is valid
valid_move(Board, Move) :-
    movementT(Board, Move, NewBoard, _),
    Board \= NewBoard.
