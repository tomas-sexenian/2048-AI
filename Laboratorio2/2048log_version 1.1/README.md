# 2048 en Prolog
# Laboratorio Programación Lógica 2023
# UdelaR
# Versión 1.1

# Versión 1.0
- Versión inicial

# Versión 1.1
- Modificación de un bug en run.py (invocaba siempre con nivel_minimax=0)


Esta interfaz permite jugar al 2048, invocando a los predicados implementados en Prolog que implementan los jugadores IA.  Está  basada en la implementación de [Walter Sebastian Gisler](https://github.com/wsgisler/flask-2048) y en [Prolog XO](https://github.com/guyzyl/prolog-tic-tac-toe), una solución para ta-te-tí que utiliza Python para hacer el puente con SWI Prolog

# Instalación
- Asegurarse de tener instalado Python (3.9+). Recomendado: instalar la distribución [Anaconda](https://www.anaconda.com/products/individual).
- Instalar las bibliotecas ejecutando `pip -m pip install requirements.txt`
- Asegurarse de que se tiene instalada la versión 2.9 de pyswip (con la 2.10 tiene problemas). 
- Asegurarse de que SWI Prolog está en el Path del sistema
- Abrir una consola, y ejecutar `python run.py` para ejecutar el servidor del juego. Debería aparecer un mensaje similar a

 * Serving Flask app "run" (lazy loading)
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: on
 * Restarting with watchdog (windowsapi)
 * Debugger is active!
 * Debugger PIN: 421-692-308
 * Running on all addresses.
   WARNING: This is a development server. Do not use it in a production deployment.
 * Running on http://172.17.248.111:5000/ (Press CTRL+C to quit)

- A partir de aquí, ir a [http://localhost:5000/<seed>/<estrategia>/<nivel_minimax>](http://localhost:5000/42/dummy/0) para jugar. El parámetro estrategia permite jugar con una estrategia con ese nombre (e.g. dummy, random, ia). Seed especifica una semilla para los números aleatorios (para que pueda reproducirse un juego). Nivel_minimax permite especificar la profundidad de búsqueda del algoritmo minimax (si se considera necesario para la implementación). 

IMPORTANTE: Si se modifica el fuente Prolog (connect4log.pl), se debe reiniciar el servidor. En los otros casos, basta con recargar la página del juego. 

# Reglas del juego

El tablero de 2048 es un damero de 4x4, donde se van agregando, luego de cada movimiento y de forma aleatoria, una nueva pieza (el tablero comienza con dos piezas).  En cada movimiento, el jugador puede deslizar las piezas hacia arriba, abajo, izquierda o derecha. Al hacer esto, las piezas se mueven tan lejos como sea posible en la dirección indicada, hasta que las detiene otra pieza o el borde del tablero. Si dos piezas del mismo número se encuentran en el movimiento, se combinan en una nueva pieza, de valor equivalente al doble del número original. La pieza resultante no puede combinarse con otra pieza, en el mismo movimento. Si tres piezas consecutivas con el mismo número se encuentran en el mismo movimiento, solamente las dos piezas ubicadas en la ubicación más lejanda del comienzo del movimiento se combinarán. 

El juego se considera ganado cuando una pieza de valor 2048 aparece en el tablero (aunque los jugadores pueden seguir jugando buscando aumentar su puntaje). El juego termina cuando no es posible realizar ningún movimiento (es decir, no hay espacios vacíos y no hay dos piezas adyacentes con el mismo valor). 

El puntaje obtenido se calcula sumando, en cada movimiento, el valor de las piezas combinadas (e.g., si en un mismo movimiento se combinan un par de 4’s  y un par de 8’s, el puntaje aumentará en 4x2+8x2 = 24).

Una implementación de referencia que puede jugarse online (y que utilizaremos para este laboratorio) está disponible en https://play2048.co/

# La interfaz

En la interfaz del juego tenemos los siguiente componentes:

- Un link para jugar en la dirección indicada
- Un link para jugar en la dirección sugerida por la estrategia seleccionada
- Un link para autoplay, con la estrategia indicada
- El puntaje hasta el momento, que se actualiza luego de cada jugada
- Un link para reiniciar el juego

# Archivos

templates/gui.html - La interfaz del juego (HTML + Javascript + Jinja)
run.py - Servidor web (usando Flask) que permite invocar (a través de un bridge) los predicados Prolog necesarios para modificar la interfaz
prolog_bridge.py - Bridge entre Python y SWI Prolog.
p2048.pl - Los predicados Prolog para el juego

Auxiliares:
- pyswip_mt.py: permite correr Prolog en un multithread. 
- requirements.txt: bibliotecas Python auxiliares
