# Código adaptado de https://github.com/wsgisler/flask-2048

from flask import Flask, escape, request, render_template
import random
from copy import deepcopy
import prolog_bridge as bridge

app = Flask(__name__)

# Convierte una matriz (lista de listas) de 4x4 en un string en el formato 0-0-0-2x0-0-2-0x0-0-0-0x0-0-0-0
# Para pasarlo a una URL
def grid_string_to_grid(grid_string):
    grid = []
    rows = grid_string.split('x')
    for row in rows:
        cols = row.split('-')
        grid.append([int(c) for c in cols])
    return grid

# Convierte el string en una lista de listas de 4x4   	
def grid_to_grid_string(grid):
    row_strings = []
    for row in grid:
        row_strings .append('-'.join([str(x) for x in row]))
    return 'x'.join(row_strings)

# Agrega el siguiente valor al tablero
# Elige un 4 solamente con probabilidad 0.2
# Depende de la semilla Y del tablero
def add_next_number(grid,seed):
    new_seed = sum(sum(x) for x in grid) + seed
    random.seed(new_seed),
    assigned = False
    while not assigned:
        row = random.choice([0,1,2,3])
        col = random.choice([0,1,2,3])
        if grid[row][col] == 0:
            assigned = True
            grid[row][col] = random.choices([2,4],[0.8,0.2])[0]
                                                    
    return grid

# Muestra el tablero   
def render_grid(g, score, game_over = False, suggested_move = None, autoplay = False,seed = 42,strategy = 'dummy' , nivel_minimax=0):
    gg = [["" if r == 0 else r for r in row] for row in g]
    return render_template('gui.html', r1c1 = gg[0][0], r1c2 = gg[0][1], r1c3 = gg[0][2], r1c4 = gg[0][3], 
                                       r2c1 = gg[1][0], r2c2 = gg[1][1], r2c3 = gg[1][2], r2c4 = gg[1][3], 
                                       r3c1 = gg[2][0], r3c2 = gg[2][1], r3c3 = gg[2][2], r3c4 = gg[2][3], 
                                       r4c1 = gg[3][0], r4c2 = gg[3][1], r4c3 = gg[3][2], r4c4 = gg[3][3], grid_encoding = grid_to_grid_string(g), score = score, game_over = game_over, suggested_move = suggested_move, autoplay = autoplay, 
                                       seed = seed, strategy = strategy, nivel_minimax=nivel_minimax)


# Realiza el movimiento. No está en uso (lo sustituimos por la llamada a Prolog)                                    
def move_grid(g, direction, current_score):
    score = current_score
    grid_copy = [[0 for c in [0,1,2,3]] for r in [0,1,2,3]]
    if direction == 'up':
        for col in [0,1,2,3]:
            syms = []
            new_syms = []
            for row in [0,1,2,3]:
                if g[row][col] != 0:
                    syms.append(g[row][col])
            skipnext = False
            for i in range(len(syms)):
                if skipnext:
                    skipnext = False
                else:
                    if i < len(syms)-1 and syms[i] == syms[i+1]:
                        new_syms.append(syms[i]*2)
                        score += syms[i]*2
                        skipnext = True
                    else:
                        new_syms.append(syms[i])
            for row in range(len(new_syms)):
                grid_copy[row][col] = new_syms[row]
    if direction == 'down':
        for col in [0,1,2,3]:
            syms = []
            new_syms = []
            for row in [3,2,1,0]:
                if g[row][col] != 0:
                    syms.append(g[row][col])
            skipnext = False
            for i in range(len(syms)):
                if skipnext:
                    skipnext = False
                else:
                    if i < len(syms)-1 and syms[i] == syms[i+1]:
                        new_syms.append(syms[i]*2)
                        score += syms[i]*2
                        skipnext = True
                    else:
                        new_syms.append(syms[i])
            if len(new_syms) > 0:
               for row in range(3,3-len(new_syms),-1):
                    grid_copy[row][col] = new_syms[3-row]
    if direction == 'left':
        for row in [0,1,2,3]:
            syms = []
            new_syms = []
            for col in [0,1,2,3]:
                if g[row][col] != 0:
                    syms.append(g[row][col])
            skipnext = False
            for i in range(len(syms)):
                if skipnext:
                    skipnext = False
                else:
                    if i < len(syms)-1 and syms[i] == syms[i+1]:
                        new_syms.append(syms[i]*2)
                        score += syms[i]*2
                        skipnext = True
                    else:
                        new_syms.append(syms[i])
            for col in range(len(new_syms)):
                grid_copy[row][col] = new_syms[col]
    if direction == 'right':
        for row in [0,1,2,3]:
            syms = []
            new_syms = []
            for col in [3,2,1,0]:
                if g[row][col] != 0:
                    syms.append(g[row][col])
            skipnext = False
            for i in range(len(syms)):
                if skipnext:
                    skipnext = False
                else:
                    if i < len(syms)-1 and syms[i] == syms[i+1]:
                        new_syms.append(syms[i]*2)
                        score += syms[i]*2
                        skipnext = True
                    else:
                        new_syms.append(syms[i])
            if len(new_syms) > 0:
               for col in range(3,3-len(new_syms),-1):
                    grid_copy[row][col] = new_syms[3-col]
    return grid_copy, score

# Realiza el movimiento
# Invoca al predicado movimientoT/4 de Prolog a través del bridge
def move_gridP(grid_string,direction,current_score):
    new_grid_string,score_gen = bridge.do_movimientoT(grid_string,direction)
    return new_grid_string,current_score+score_gen

# Chequea si se terminó el juego por no haber movimientos posibles
def game_over(grid):
    game_over = True
    counter = 0
    has_two_same = False
    for r in [0,1,2,3]:
        for c in [0,1,2,3]:
            counter += 1 if grid[r][c] != 0 else 0
            if r < 3 and grid[r][c] == grid[r+1][c]:
                game_over = False
                break
            if c < 3 and grid[r][c] == grid[r][c+1]:
                game_over = False
                break
    if counter == 16 and game_over:
        return True
    return False
    
# Cuenta las celdas vacías en el tablero
def count_empty(grid):
    counter = 0
    for row in grid:
        for col in row:
            counter += 1 if col == 0 else 0
    return counter

# Sugiere un movimiento. Versión original
def suggest_move(original, num_simulations, depth,seed):
    """ play some scenarios and determine which one has the best expected outcome """
    directions = ['up','down','right','left']
    total_score = {d:0 for d in directions}
    total_deaths = {d:0 for d in directions}
    expected_empty = {d:0 for d in directions}
    min_number_moves_to_kill = {d:1000 for d in directions}
    for direction in directions:
        griddd = deepcopy(original)
        score = 0
        gridd, score = move_grid(griddd, direction, score)
        gridd = deepcopy(gridd)
        if grid_to_grid_string(gridd) != grid_to_grid_string(original):
            for i in range(num_simulations):
                grid = deepcopy(gridd)
                grid = add_next_number(grid,seed)
                d = 1
                deathcount = depth
                while(d <= depth):
                    dir = random.choice(['up','down','right','left'])
                    prev_encoding = grid_to_grid_string(grid)
                    grid, score = move_grid(grid, dir, score)
                    if grid_to_grid_string(grid) != prev_encoding:
                        grid = add_next_number(grid,seed)
                        d += 1
                    else:
                        break
                    if game_over(grid):
                        total_deaths[direction] += 1
                        deathcount = d
                        break
                min_number_moves_to_kill[direction]
                total_score[direction] += score
                min_number_moves_to_kill[direction] = min(min_number_moves_to_kill[direction], deathcount)
                expected_empty[direction] += count_empty(grid)
    for key in total_deaths:
        if total_deaths[key] == 0:
            total_deaths[key] = 10000000000000000
    for key in min_number_moves_to_kill:
        if min_number_moves_to_kill[key] == 1000:
            min_number_moves_to_kill[key] = 0
    # Now we have to use one of the strategies
    #return min(total_deaths, key=total_deaths.get)
    return max(total_score, key=total_score.get)
    #return max(min_number_moves_to_kill, key=min_number_moves_to_kill.get)
    #return max(expected_empty, key=expected_empty.get)

# Sugiere un movimiento
# Invoca al predicado mejor_movimiento/4 de Prolog
# Con la estrategia que corresponda
def suggest_moveP(grid_string,nivel_minimax,estrategia):
    direction = bridge.do_mejor_movimiento(grid_string,nivel_minimax,estrategia)
    return direction


# Pantalla inicial. Recibe la semilla aleatoria en los parámetros, y la estrategia
@app.route('/<int:seed>/<string:strategy>/<int:nivel_minimax>')
def index(seed,strategy, nivel_minimax):
    start_grid = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
    start_grid = add_next_number(start_grid,seed)
    start_grid = add_next_number(start_grid,seed)
    grid_string = grid_to_grid_string(start_grid)
    suggested_move = suggest_moveP(grid_string, 0, strategy)
    return render_grid(start_grid,score=0,suggested_move=suggested_move, seed=seed,strategy=strategy, nivel_minimax = nivel_minimax)

# Tablero con movimiento indicado. No se utiliza. 
@app.route('/move/<int:seed>,<string:direction>/<string:last_encoding>/<int:score>/')
def move(seed,direction, last_encoding, score):

    grid = grid_string_to_grid(last_encoding)
    grid, score = move_grid(grid, direction, score)
    if last_encoding != grid_to_grid_string(grid):
        grid = add_next_number(grid,seed)
    suggested_move = suggest_move(grid, 200, 9,seed)
    if game_over(grid):
        return render_grid(grid, score, True, suggested_move,seed=seed)
    return render_grid(grid, score, False, suggested_move, seed=seed)
    
# Invoco a Prolog para realizar un movimiento
# Le paso el movimiento, el encoding del tablero y el score actual
@app.route('/moveP/<int:seed>/<string:strategy>/<int:nivel_minimax>/<string:direction>/<string:last_encoding>/<int:score>/')
def moveP(seed, strategy, nivel_minimax, direction, last_encoding, score):

    grid_string, score = move_gridP(last_encoding, direction, score)

    grid = grid_string_to_grid(grid_string)
    if last_encoding != grid_string:
        grid = add_next_number(grid,seed)
    
    grid_string = grid_to_grid_string(grid)

    suggested_move = suggest_moveP(grid_string, nivel_minimax, strategy)
    if game_over(grid):
        return render_grid(grid, score, True, suggested_move, seed=seed, strategy = strategy, nivel_minimax=nivel_minimax )
    return render_grid(grid, score, False, suggested_move,seed=seed, strategy = strategy, nivel_minimax = nivel_minimax)


# Autoplay. Versión original, no se usa.
@app.route('/autoplay/<int:seed>/<string:direction>/<string:last_encoding>/<int:score>/')
def automove(seed,direction, last_encoding, score):
    grid = grid_string_to_grid(last_encoding)
    grid, score = move_grid(grid, direction, score)
    if last_encoding != grid_to_grid_string(grid):
        grid = add_next_number(grid,seed)
    suggested_move = suggest_move(grid, 200, 9,seed)
    if game_over(grid):
        return render_grid(grid, score, True, suggested_move,seed = seed)
    return render_grid(grid, score, False, suggested_move, autoplay = True, seed=seed)

# Autoplay con Prolog
@app.route('/autoplayP/<int:seed>/<string:strategy>/<int:nivel_minimax>/<string:direction>/<string:last_encoding>/<int:score>/')
def automoveP(seed,strategy,nivel_minimax,direction, last_encoding, score):

    # Realizo el movimiento prolog
    # Retorna el grid_string del tablero resultado del movimiento y el nuevo score
    grid_string, score = move_gridP(last_encoding, direction, score)

    # Obtengo el grid a partir del string
    grid = grid_string_to_grid(grid_string)
    
    if last_encoding != grid_string:
        grid = add_next_number(grid,seed)

    # Obtengo el nuevo grid
    grid_string = grid_to_grid_string(grid)

    # Movimiento sugerido
    suggested_move = suggest_moveP(grid_string, nivel_minimax,strategy)
    if game_over(grid):
        return render_grid(grid, score, True, suggested_move,seed = seed, strategy = strategy, nivel_minimax = nivel_minimax)
    return render_grid(grid, score, False, suggested_move, autoplay = True, seed=seed, strategy = strategy, nivel_minimax = nivel_minimax)
    
if __name__ == "__main__":
    app.run(debug=True, port = 5000, host = "0.0.0.0")