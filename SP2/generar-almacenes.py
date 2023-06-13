import csv
import numpy as np
import random
import sys
import os

NOMBRES = ["Destornillador","Martillo","Alicate","Serrucho","Tornillo","Clavo","Cable","Tuerca","Arduino","Raspberry","OrangePi","IntelGalileo","Boligrafo","Goma","Sacapuntas","Tijeras","Pegamento","Cuaderno","Carpeta","Lapiz","JetsonNano"]
CANT_MAX_ITEM = 100
PRECIO_MAX_ITEM = 1000
MAX_ITEMS_POR_CELDA = 10
MAX_ROWS = 10
MAX_COLS = 10
MAX_QUERIES = 10

def generarItem():
    nombre = random.choice(NOMBRES)
    cantidad = random.randint(1,CANT_MAX_ITEM)
    precio = random.randint(1,PRECIO_MAX_ITEM)
    return {"nombre":nombre,"cantidad":cantidad,"precio":precio}

def generarCelda():
    items = {}
    for i in range(random.randint(1,MAX_ITEMS_POR_CELDA)):
        item = generarItem()
        items[item["nombre"]] = item
    return items

def num2Cords(num):
    row = num // MAX_ROWS
    col = num % MAX_COLS
    return (row,col)

def generarAlmacen():
    almacen = {}
    for i in range(MAX_ROWS*MAX_COLS):
        almacen[num2Cords(i)] = generarCelda()
    return almacen

POSIBLES_INSTRUCCIONES = ["remove", "add", "move"]
MOVE_DIRECTIONS = ["up", "down", "left", "right"]

def generarMov():
    return random.choice(MOVE_DIRECTIONS)

def generarRemove(cords, almacen):
    celda = almacen[cords]
    opciones = list(celda.keys())
    if len(opciones) == 0:
        print("No hay items en la celda")
        return Exception("No hay items en la celda")
    item = random.choice(opciones)
    cant_max = celda[item]["cantidad"]
    print(cant_max)
    cant = random.randint(1,cant_max)
    return (item,cant)

def generarAdd(cords, almacen):
    celda = almacen[cords]
    opciones = list(celda.keys())
    if len(opciones) == 0:
        print("No hay items en la celda")
        return Exception("No hay items en la celda")
    item = random.choice(opciones)
    cant_max = celda[item]["cantidad"]
    cant = random.randint(1,cant_max)
    return (item,cant)

def generarInstruccion(cords, almacen):
    instruccion = random.choice(POSIBLES_INSTRUCCIONES)
    if instruccion == "move":
        mov = generarMov()
        if(mov == "up"):
            cords = (cords[0]-1 % MAX_ROWS, cords[1])
        elif(mov == "down"):
            cords = (cords[0]+1 % MAX_ROWS, cords[1])
        elif(mov == "left"):
            cords = (cords[0], cords[1]-1 % MAX_COLS)
        elif(mov == "right"):
            cords = (cords[0], cords[1]+1 % MAX_COLS)
        query = instruccion + " " + mov
        return almacen, cords, query
    elif instruccion == "remove":
        item, cant = generarRemove(cords, almacen)
        query = instruccion + " " + item + " " + str(cant)
        almacen[cords][item]["cantidad"] -= cant
        return almacen, cords, query 
    elif instruccion == "add":
        item, cant = generarAdd(cords, almacen)
        query = instruccion + " " + item + " " + str(cant)
        almacen[cords][item]["cantidad"] += cant
        return almacen, cords, query
    
def generarQueries(almacen):
    cant_queries = random.randint(1,MAX_QUERIES)
    cords = (0,0)
    queries = ""
    for i in range(cant_queries):
        almacen, cords, query = generarInstruccion(cords, almacen)
        queries += ";" + query
    return queries


def main():
    num_of_almacenes = len(os.listdir("outputs"))
    almacen = generarAlmacen()
    print(almacen)
    queries = generarQueries(almacen)
    with open("outputs/"+str(num_of_almacenes)+".csv", "w") as f:
        # first row: rows, cols, queries
        #then each cell with its items with the next format:
        # ex: Destornillador.10.5;Martillo.5.20;Alicate.4.15
        f.write(str(MAX_ROWS) + "," + str(MAX_COLS) + "," + queries + "\n")
        for i in range(MAX_ROWS):
            for j in range(MAX_COLS):
                celda = almacen[(i,j)]
                for item in celda:
                    f.write(item + "." + str(celda[item]["cantidad"]) + "." + str(celda[item]["precio"]) + ";")
                f.write(",")
            f.write("\n")


if __name__ == "__main__":
    main()