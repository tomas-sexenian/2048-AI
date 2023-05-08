%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Unit tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tableros de ejemplo

% Tablero genérico 
tablero(1,m(f(-,-,-,-),f(-,2,-,16),f(-,2,-,16),f(2,4,8,8))).
% Tablero lleno
tablero(2,m(f(2,4,8,16),f(16,2,4,16),f(2,2,4,16),f(2,4,8,8))).

% No tiene movimiento posible hacia arriba
tablero(3,m(f(2,-,-,2),f(-,-,-,-),f(-,-,-,-),f(-,-,-,-))).

% Tablero sin movimiento posible hacia arriba
tablero(4,m(f(2,-,2,-),f(-,-,-,-),f(-,-,-,-),f(-,-,-,-))).

% 
tablero(5,m(f(-,256,-,-),f(-,256,1024,1024),f(-,-,2,-),f(-,4,4,4))).


:- begin_tests(lab2).

%%%% Evaluación de los movimientos

% up
test(mov1,[nondet]):-
	tablero(1,Tablero),
	movimientoT(Tablero,up,TableroNew,36),
	TableroNew = m(f(2, 4, 8, 32), f(-, 4, -, 8), f(-, -, -, -), f(-, -, -, -)).

% down
test(mov2,[nondet]):-
	tablero(1,Tablero),
	movimientoT(Tablero,down,TableroNew,36),
	TableroNew =  m(f(-, -, -, -), f(-, -, -, -), f(-, 4, -, 32), f(2, 4, 8, 8)).

% left 
test(mov3,[nondet]):-
	tablero(1,Tablero),
	movimientoT(Tablero,left,TableroNew,16),
	TableroNew =   m(f(-, -, -, -), f(2, 16, -, -), f(2, 16, -, -), f(2, 4, 16, -)).

% right
test(mov4,[nondet]):-
	tablero(1,Tablero),
	movimientoT(Tablero,right,TableroNew,16),
	TableroNew =   m(f(-, -, -, -), f(-, -, 2, 16), f(-, -, 2, 16), f(-, 2, 4, 16)).

% No hay movimiento posible, debería quedar igual y devolver 0
test(mov5,[nondet]):-
	tablero(3,Tablero),
	movimientoT(Tablero,up,TableroNew,0),
	TableroNew =   m(f(2,-,-,2),f(-,-,-,-),f(-,-,-,-),f(-,-,-,-)).

test(mov6,[nondet]):-
    tablero(5,Tablero),
    movimientoT(Tablero,up,_,512).

test(mov6,[nondet]):-
    tablero(5,Tablero),
    movimientoT(Tablero,left,_,2056).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Estrategia dummy

test(dummy1,[nondet]):-
	tablero(1,Tablero),
	mejor_movimiento(Tablero,_,dummy,up).

% No tiene movimiento up
test(dummy2,[nondet]):-
	tablero(4,Tablero),
	mejor_movimiento(Tablero,_,dummy,left).


test(dummy3,[nondet]):-
    tablero(5,Tablero),
    mejor_movimiento(Tablero,_,dummy,left).
:- end_tests(lab2).

