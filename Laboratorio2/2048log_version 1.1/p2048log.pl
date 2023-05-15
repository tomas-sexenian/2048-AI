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

% The movementT predicate applies a move to the board and calculates the new score
movementT(Board, left, BoardNew, ScoreGen) :-
    maplist(shift_left, Board, Board1),
    maplist(merge_left, Board1, BoardNew),
    score(Board, BoardNew, ScoreGen).

movementT(Board, right, BoardNew, ScoreGen) :-
    maplist(reverse, Board, BoardRev),
    maplist(shift_left, BoardRev, Board1Rev),
    maplist(merge_left, Board1Rev, Board2Rev),
    maplist(shift_left, Board2Rev, Board3Rev),
    maplist(reverse, Board3Rev, BoardNew),
    score(Board, BoardNew, ScoreGen).

movementT(Board, up, BoardNew, ScoreGen) :-
    rotate_left(Board, BoardRot),
    maplist(shift_left, BoardRot, Board1Rot),
    maplist(merge_left, Board1Rot, Board2Rot),
    maplist(shift_left, Board2Rot, Board3Rot),
    rotate_right(Board3Rot, BoardNew),
    score(Board, BoardNew, ScoreGen).

movementT(Board, down, BoardNew, ScoreGen) :-
    rotate_right(Board, BoardRot),
    maplist(shift_left, BoardRot, Board1Rot),
    maplist(merge_left, Board1Rot, Board2Rot),
    maplist(shift_left, Board2Rot, Board3Rot),
    rotate_left(Board3Rot, BoardNew),
    score(Board, BoardNew, ScoreGen).

% The 'random' strategy just picks a move at random
mejor_movimiento(_, _, random, Move) :-
    random_member(Move, [up, down, left, right]).

% The 'dummy' strategy picks the move that results in the highest immediate score
mejor_movimiento(Board, _, dummy, Move) :-
    findall(Score-Move, (member(Move, [up, down, left, right]), movementT(Board, Move, _, Score)), ScoresMoves),
    keysort(ScoresMoves, SortedScoresMoves),
    reverse(SortedScoresMoves, [_-Move|_]).

% The 'ia' strategy uses the minimax algorithm to select the best move
mejor_movimiento(Board, Depth, ia, Move) :-
    findall(Score-Move, (member(Move, [up, down, left, right]), movementT(Board, Move, BoardNew, _), minimax(BoardNew, Depth, Score)), ScoresMoves),
    keysort(ScoresMoves, SortedScoresMoves),
    reverse(SortedScoresMoves, [_-Move|_]).
