# -*- coding: utf-8 -*-
"""
Python function to bridge between the server and the Prolog logic.
Basado en https://github.com/guyzyl/prolog-tic-tac-toe
"""
from os import path
from typing import List, Optional
from itertools import islice
import re
from numpy import character
from pyparsing import Char
from jinja2 import Template

from pyswip_mt import PrologMT


BASE_PROLOG_FILE = 'p2048log.pl'
STACK_LIMIT = 4000000000    # Limit to about 30 seconds runtime

prolog = PrologMT()
currently_consulted = ""


# Leo el código Prolog
def consultar_reglas_juego() -> None:

    archivo_reglas= f"{BASE_PROLOG_FILE}"
    # Check if file matches current one being used
    global currently_consulted
    if archivo_reglas == currently_consulted:
        return

    next(prolog.query(f"set_prolog_flag(stack_limit, {STACK_LIMIT})."))
    next(prolog.query(f'unload_file("{currently_consulted}").'))
    prolog.consult(archivo_reglas)
    currently_consulted = archivo_reglas


# Ejecuta un movimiento
# Devuelve el puntaje generado, además del nuevo tablero
def do_movimientoT(tablero:str,direction:str) -> str:

    consultar_reglas_juego()    


    # Convierte el string del tablero al formato de estructura que espera Prolog
    tablero_prolog = tablero_to_prolog(tablero)
    
    # Invoca al predicado movimientoT
    prolog_query = f"movimientoT({tablero_prolog},{direction},TableroNew,ScoreGen)."
    #print(prolog_query)
  
    query_result = list(prolog.query(prolog_query, maxresult=1))
  

    if (query_result == []):
        print("Oops! Problema al ejecutar el movimiento")
    else:
        tablero_new = query_result[0].get("TableroNew")
        score_gen = query_result[0].get("ScoreGen")
    
    tablero_str_new = prolog_to_tablero(tablero_new)

    return tablero_str_new,score_gen


# Recibe un tablero que es un string con el formato de la interfaz
# Y retorna un string en el formato que Prolog espera
#    tablero: 0-0-0-0x0-2-0-16x0-2-0-16x2-4-8-8
#    Devuelve: 'm(f(-,-,-,-),f(-,2,-,16),f(-,2,-,16),f(2,4,8,8))'.
def tablero_to_prolog(tablero_str: str) -> str:

    valores = re.split(r"-|x",tablero_str)
    pos_tablero=0
    tablero_prolog = 'm('
    for fila in range(4):
        tablero_prolog +='f('
        for col in range(4):
            if valores[pos_tablero] != "0":
                tablero_prolog+=str(valores[pos_tablero])+','
            else:
                tablero_prolog+='-,'
            pos_tablero += 1
        tablero_prolog = tablero_prolog[:-1]
        tablero_prolog +='),'

    tablero_prolog = tablero_prolog[:-1]
    tablero_prolog += ')'

    return tablero_prolog
    

# Recibe un tablero como una matriz functor y lo convierte al formato de la interfaz
def prolog_to_tablero(tablero_prolog:str) -> str:

    tablero_prolog = tablero_prolog.replace(" ","")

    # Identifico cada una de las posiciones cada una de las posiciones
    # re.group(i) queda con el valor de la posición
    r=re.search('m\(' +
            'f\(([0-9\-]+)\,([0-9\-]+)\,([0-9\-]+)\,([0-9\-]+)\)\,'+
            'f\(([0-9\-]+)\,([0-9\-]+)\,([0-9\-]+)\,([0-9\-]+)\)\,'+
            'f\(([0-9\-]+)\,([0-9\-]+)\,([0-9\-]+)\,([0-9\-]+)\)\,'+
            'f\(([0-9\-]+)\,([0-9\-]+)\,([0-9\-]+)\,([0-9\-]+)\)'+
            '\)',tablero_prolog)

    # Armo el string directamente
    tablero_str=''
    pos=1
    for row in range(4):
        for col in range(4): 
            if r.group(pos)=='-':
                symbol = '0'
            else:
                symbol = r.group(pos)
            tablero_str += symbol
            if col<3:
                tablero_str += '-'
            else:
                tablero_str += 'x'
            pos += 1
    return tablero_str[:-1]


# Recibe el tablero (lista de listas), así como la estrategia a utilizar
# Devuelve en Jugada la dirección en la que sugiere jugar
def do_mejor_movimiento(tablero:str,nivel_minimax:int,estrategia:str) -> str:
    consultar_reglas_juego()
 
    tablero_prolog = tablero_to_prolog(tablero)
    prolog_query =  f"mejor_movimiento({tablero_prolog},{nivel_minimax},{estrategia},Jugada)."

    query_result = list(prolog.query(prolog_query, maxresult=1))
    if (query_result == []):
        mejor_jugada = 'oops'
    else:
        mejor_jugada = query_result[0].get("Jugada")

    return mejor_jugada