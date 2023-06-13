; Iván Alberto Romero Wells A00833623
; ---------------------- Librerías ----------------------
; Importación de librerías útiles en la realización del programa, principalmente para el manejo de archivos csv
(ns main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

; ---------------------- Lectura de Almacén ----------------------
; Función para leer un archivo csv y retornar una matriz con los datos
(defn read-csv [filename]
  (with-open [reader (io/reader filename)]
    (doall (csv/read-csv reader))))

; Función para escribir un archivo csv a partir de una matriz de datos
(defn write-csv [data filename]
  (with-open [w (io/writer filename)]
    (csv/write-csv w data)))

; Dividir una celda en sus items correspondientes por ;
; Se recibe una celda en string y se retorna una lista con los items
(defn cell-to-items [cell]
  (clojure.string/split cell #";"))

; Dividir un item en sus atributos correspondientes por :
; Se recibe un item en string y se retorna una lista con los atributos
(defn item-to-list [item]
    (clojure.string/split item #":"))

; Agrupar los atributos de un item en un mapa
; Se recibe un item en lista ordenada y se retorna un mapa con los atributos
(defn cell-to-map [cell]
  (zipmap '(Nombre Cantidad Precio) cell))

; Agrupar los atributos de una celda en una lista de mapas
; Se recibe una celda en string y se retorna una lista de mapas con los atributos de cada item
(defn raw-cell-to-map [cell]
    (let [splited-cell (cell-to-items cell)
        items (map #(str/split % #"\.") splited-cell)
        maps (map cell-to-map items)
        ]
        maps))

; Agrupar los atributos de una fila en una lista de listas de mapas
(defn row-to-map [row]
(map raw-cell-to-map row))

; Agrupar los atributos de una matriz en una lista de listas de listas de mapas
(defn matrix-to-maps [matrix]
    (map row-to-map matrix))

; Agrupar los atributos de una celda en un mapa de mapas
(defn cell-to-map-of-maps [cell]
  (zipmap (map #(get % 'Nombre) cell) cell))

; Agrupar los atributos de una fila en un mapa de mapas
(defn row-to-map-of-maps [row]
  (map cell-to-map-of-maps row))

; Agrupar los atributos de una matriz en un mapa de mapas
(defn matrix-to-map-of-maps [matrix]
    (map row-to-map-of-maps matrix))

; Desimbrincar una lista de listas de mapas en una lista de mapas
(defn des-imbrincar-lista [lista]
  (map #(apply merge %) lista))

; Desimbrincar una lista de mapas en un mapa
(defn des-imbrincar [lista]
  (apply concat lista))

; Convertir un número en sus cordenadas correspondientes en base a las dimensiones del almacen
(defn count-to-cord [count cols rows]
    [(mod count (Integer/parseInt cols)) (int (/ count (Integer/parseInt cols)))])

; Asociar las coordenadas de cada celda en un mapa de mapas
(defn associate-coordinates [matrix cols rows]
    (zipmap (map #(count-to-cord % cols rows) (range (count matrix))) matrix))

; Crear un mapa de estado a partir de un archivo csv
(defn almacen-from-csv [filename cols rows]
    (let [
        mat-maps (matrix-to-maps (rest (read-csv filename)))
        maps (matrix-to-map-of-maps mat-maps)
        unimbrincado (des-imbrincar maps)
    ]
        (associate-coordinates unimbrincado cols rows)
    ))

; Genera la representación del autómata desde un archivo csv
(defn state-map-from-csv [filename cols rows]
    (hash-map 'estado [0 0] 'almacen (almacen-from-csv filename cols rows)))

; ---------------------- Funciones de Autómata ----------------------

; Función para primera transición del autómata
; Describe las transiciones de movimiento del almacen
(defn move [state-map direction rows cols]
    (let [estado (get state-map 'estado)
          almacen (get state-map 'almacen)
          x (first estado)
          y (second estado)
          new-cords (case direction
                        "up" [x (mod (dec y) (Integer/parseInt rows))]
                        "down" [x (mod (inc y) (Integer/parseInt rows))]
                        "left" [(mod (dec x) (Integer/parseInt cols)) y]
                        "right" [(mod (inc x) (Integer/parseInt cols)) y])]
        (hash-map 'estado new-cords 'almacen almacen)))

; DEBUG
; Permite ver el estado actual del autómata
(defn show-actual-cell [state-map]
    (get (get state-map 'almacen) (get state-map 'estado)))

; Función para agregar una cantidad determinada de un producto a la celda actual
; Se recibe el estado actual del autómata, el nombre del producto y la cantidad a agregar
; Esta función encuentra el producto dentro de la celda y actualiza su cantidad
(defn add-product [state-map name quantity]
    (let [almacen (get state-map 'almacen)
          estado (get state-map 'estado)
          actual-cell (get almacen estado)
          product (get actual-cell name)
          actual-quantity (if (= (type (get product 'Cantidad)) java.lang.Long) (get product 'Cantidad) (Integer/parseInt (get product 'Cantidad)))
          new-product (assoc product 'Cantidad (+ actual-quantity quantity))
          new-cell (assoc actual-cell name new-product)]
        (assoc state-map 'almacen (assoc almacen estado new-cell))))

; Función para remover una cantidad determinada de un producto a la celda actual
; Se recibe el estado actual del autómata, el nombre del producto y la cantidad a remover
; Esta función encuentra el producto dentro de la celda y actualiza su cantidad
(defn remove-product [state-map name quantity]
    (let [almacen (get state-map 'almacen)
          estado (get state-map 'estado)
          actual-cell (get almacen estado)
          product (get actual-cell name)
          actual-quantity (if (= (type (get product 'Cantidad)) java.lang.Long) (get product 'Cantidad) (Integer/parseInt (get product 'Cantidad)))
          new-product (assoc product 'Cantidad (- actual-quantity quantity))
          new-cell (assoc actual-cell name new-product)]
        (assoc state-map 'almacen (assoc almacen estado new-cell))))

; ---------------------- Ejecución de Queries ----------------------

; Función para hacer la ejecución de una consulta
; Filtra la consulta de string a la acción a realizar, esto permite dividir claramente los casos en las acciones a realizar
; con esto se puede llamar a la función correspondiente para cada caso
(defn exec-query [state-map query rows cols]
    (let [query-list (clojure.string/split query #" ")
          command (first query-list)]
          (case command
            "move" (do 
                    ;; (println "Move")
                    ;; (println (first (rest query-list)))
                    (move state-map (first (rest query-list)) rows cols))
            "add" (do 
                    ;; (println "Add")
                    ;; (println (first (rest query-list)))
                    ;; (println (first (rest (rest query-list))))
                    (add-product state-map (first (rest query-list)) (Integer/parseInt (first (rest (rest query-list))))))
            "remove" (do
                    ;; (println "Remove")
                    ;; (println (first (rest query-list)))
                    ;; (println (first (rest (rest query-list))))
                    (remove-product state-map (first (rest  query-list)) (Integer/parseInt (first (rest (rest query-list))))))
            "show" (do 
                    (println "Show")
                    (println (show-actual-cell state-map))
                          state-map))
            ))


; Función para ejecutar una lista de consultas
; Esta función recursiva ejecuta cada consulta de la lista de consultas
(defn exec-queries [state-map queries rows cols]
    (if (empty? queries)
        state-map
        (exec-queries (exec-query state-map (first queries) rows cols) (rest queries) rows cols))
    )

; ---------------------- Funciones de Transformación a csv ----------------------
; Pasar de HashMap a lista para cada item
(defn item-to-list [item]
    (list (get item 'Nombre) (get item 'Cantidad) (get item 'Precio)))

; Función para transformar una lista de items a string
(defn itemlist-to-string [itemlist]
    (str/join "." itemlist))

; Función para transformar una celda a string
(defn cell-to-string [cell]
    (let [keys (keys cell)
          values (vals cell)
          items (map item-to-list values)
          items-string (map itemlist-to-string items)]
        (str/join ";" items-string)))

; Función para transformar un almacen a string
(defn write-row-to-list [almacen row-num rows cols]
    (let [starting-cell (* row-num (Integer/parseInt cols))]
        (map #(cell-to-string (get almacen (count-to-cord (+ starting-cell %) cols rows))) (range (Integer/parseInt cols)))))

; Función para transformar un almacen a string
(defn write-almacen-to-list [almacen almacen-rows almacen-cols]
    (map #(write-row-to-list almacen % almacen-rows almacen-cols) (range (Integer/parseInt almacen-rows))))

; Función para transformar un almacen a string
(defn almacen-list-to-csv [almacen-list]
    (almacen-list)
)

; Función para transformar un almacen a csv
(defn write-to-csv [filename almacen-list]
    (csv/write-csv filename almacen-list)
)

; ---------------------- Evaluación de Estadísticas ----------------------
; Función para evaluar el valor del item con respecto a su cantidad y precio
(defn valor-item [item]
    (let [cantidad (if (= (type (get item 'Cantidad)) java.lang.Long) (get item 'Cantidad) (Integer/parseInt (get item 'Cantidad)))
          precio (if (= (type (get item 'Precio)) java.lang.Long) (get item 'Precio) (Integer/parseInt (get item 'Precio)))]
        (* cantidad precio)))

; Función para evaluar el valor de una celda con respecto a sus items
(defn valor-total-celda [celda]
    (let [keys (keys celda)
          values (vals celda)
          items (map valor-item values)]
        (reduce + items)))

; Función para evaluar el valor de un almacen con respecto a sus celdas
(defn valor-total-almacen [almacen]
    (let [
        celdas-keys (keys almacen)
        celdas-values (vals almacen)
        celdas-valores (map valor-total-celda celdas-values)]
        (reduce + celdas-valores)))

; Función para evaluar el valor del inventario de un item
(defn inventario-item [item]
    (if (= (type (get item 'Cantidad)) java.lang.Long) (get item 'Cantidad) (Integer/parseInt (get item 'Cantidad)))
)

; Función para evaluar el valor del inventario de una celda
(defn inventario-celda [celda]
    (let [keys (keys celda)
          values (vals celda)
          items (map inventario-item values)]
        (reduce + items)))

; Función para evaluar el valor del inventario de un almacen
(defn inventario-almacen [almacen]
    (let [
        celdas-keys (keys almacen)
        celdas-values (vals almacen)
        celdas-valores (map inventario-celda celdas-values)]
        (reduce + celdas-valores)))

; Función para determinar los items faltantes en una celda
(defn items-faltantes [celda]
    (let [keys (keys celda)
          values (vals celda)
          items (filter #(< (if (= (type (get % 'Cantidad)) java.lang.Long) (get % 'Cantidad) (Integer/parseInt (get % 'Cantidad))) 5) values)]
        (map #(get % 'Nombre) items)))

; Función para determinar los items faltantes en un almacen
(defn items-faltantes-almacen [almacen]
    (let [
        celdas-keys (keys almacen)
        celdas-values (vals almacen)
        celdas-valores (map items-faltantes celdas-values)]
        (reduce concat celdas-valores)))

; ---------------------- Funciones de Ejecución de Procesos ----------------------
; Función para ejecutar un proceso completo de un almacen
; Esta función ejecuta todos los procesos correspondientes a un almacén
; Lee el archivo y crea el autómata, posteriormente ejecuta las consultas y finalmente escribe el archivo de salida
; Así mismo, retorna un mapa con los valores de evaluación de este almacén individual, para posteriormente poder evaluar el valor total del inventario
(defn exec-process [filename output-filename]
    (let [cols (first (first (read-csv filename)))
          rows (first (rest (first (read-csv filename))))
          state-map (state-map-from-csv filename cols rows)
          queries (str/split (first (rest (rest (first (read-csv filename))))) #";")
          final-state (exec-queries state-map queries rows cols)
          valor-total (valor-total-almacen (get final-state 'almacen))
          inventario-total (inventario-almacen (get final-state 'almacen))
          inventario-faltantes (items-faltantes-almacen (get final-state 'almacen))
            ]
        (do 
            (write-csv (write-almacen-to-list (get final-state 'almacen) rows cols) output-filename)
            {'ValorTotal valor-total 'InventarioTotal inventario-total 'InventarioFaltantes inventario-faltantes 'AlmacenName filename}
            )))

(def lista-input-output 
(list (list "data.csv" "output.csv") (list "almacenes/0.csv" "outputs/0.csv") (list "almacenes/1.csv" "outputs/1.csv") (list "almacenes/2.csv" "outputs/2.csv") (list "almacenes/3.csv" "outputs/3.csv") (list "almacenes/4.csv" "outputs/4.csv") (list "almacenes/5.csv" "outputs/5.csv")))
(println lista-input-output)
(def outputs (pmap #(exec-process (first %) (second %)) lista-input-output))

; Obtención de valores para el informe

; ---------------------- Funciones de Obtención de Valores para el Informe ----------------------
; Valor total del inventario
(defn valor-total-inventario [outputs]
    (let [valor-total (map #(get % 'ValorTotal) outputs)]
        (reduce + valor-total)))

; Valor total del inventario por almacén
(defn get-items-faltantes-with-almacen [outputs]
    (let [almacenes (map #(get % 'AlmacenName) outputs)
          items-faltantes (map #(get % 'InventarioFaltantes) outputs)]
        (map #(zipmap almacenes %) items-faltantes)))

; Valor del inventario de un almacén más alto 
(defn get-max-inventario [outputs]
    (let [inventarios (map #(get % 'InventarioTotal) outputs)]
        (apply max inventarios)))

; Almacén con el inventario más alto
(defn get-inventario-max-almacen [outputs max-valor-total]
    (let [almacenes (map #(get % 'AlmacenName) outputs)
          inventarios (map #(get % 'InventarioTotal) outputs)
          max-almacen (first (filter #(= (get % 'InventarioTotal) max-valor-total) outputs))]
        (get max-almacen 'AlmacenName)))


; ---------------------- Despliegue de Resultados ----------------------
; Despliegue de resultados
(def inventario-total (valor-total-inventario outputs))
(def items-faltantes (get-items-faltantes-with-almacen outputs))
(def max-inventario (get-max-inventario outputs))

(println "Valor total: ")
(println inventario-total)
(println "Items faltantes: ")
(println items-faltantes)
(println "Max inventario: ")
(println max-inventario)
(println (get-inventario-max-almacen outputs max-inventario))


;(println (write-almacen-to-list (get state-map 'almacen) rows cols))