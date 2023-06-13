(ns main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn read-csv [filename]
  (with-open [reader (io/reader filename)]
    (doall (csv/read-csv reader))))

(defn write-csv [data filename]
  (with-open [w (io/writer filename)]
    (csv/write-csv w data)))

;; (def data (read-csv "data.csv"))
;; (def cols (first (first data)))
;; (def rows (first (rest (first data))))
;; (println rows)
;; ; 3
;; (println cols)
;; ; 4
;; (println (first(rest data)))
;; (def first_row (first(rest data)))
; [Destornillador.10.5;Martillo.5.20;Alicate.4.15 Taladro.2.100;Serrucho.3.80 Pinza.2.50;Tornillo.100.5;Clavo.200.1 Alambre.100.2;Cable.50.3;Tuerca.100.1]

; split cell by ;
(defn cell-to-items [cell]
  (clojure.string/split cell #";"))

; then split each item by .
(defn item-to-list [item]
    (clojure.string/split item #":"))

; cell to map
(defn cell-to-map [cell]
  (zipmap '(Nombre Cantidad Precio) cell))

; test on first cell
;; (println (cell-to-items (first first_row)))
;; (println (type (cell-to-items (first first_row))))
;; (println (first (cell-to-items (first first_row))))
;; (println (type (first (cell-to-items (first first_row)))))
;; (println (item-to-list (first (cell-to-items (first first_row)))))

; convert each cell into a list of items that each item is a list containing Name, Quantity, Price
; Apply cell-to-items and then item-to-list to each cell


;(println (cell-to-list ((first first_row))))
;(println (cell-to-items (first first_row)))
;; (println (item-to-list (first (cell-to-items (first first_row)))))

;(println (cell-to-map (item-to-list (first (cell-to-items (first first_row))))))

(defn raw-cell-to-map [cell]
    (let [splited-cell (cell-to-items cell)
        items (map #(str/split % #"\.") splited-cell)
        maps (map cell-to-map items)
        ]
        maps))
    ;; (do (println "raw-cell-to-map")
    ;;     (println "cell " cell)
    ;;     (println "cell to items " (cell-to-items cell))
    ;;     (println "test " (str/split (first (cell-to-items cell)) #"\."))
    ;;     (println "indv cell to items" (item-to-list (first (cell-to-items cell))))
    ;;     (println "item to list" (map item-to-list (cell-to-items cell)))
    ;;     (println (map cell-to-map (map item-to-list (cell-to-items cell))))
    ;;     (map cell-to-map (map item-to-list (cell-to-items cell)))))
    ;(map cell-to-map (map item-to-list (cell-to-items cell))))

;(println (raw-cell-to-map (first first_row)))
(defn row-to-map [row]
(map raw-cell-to-map row))

(defn matrix-to-maps [matrix]
    (map row-to-map matrix))

;(println (row-to-map (first (rest data))))
;(println (matrix-to-maps (rest data)))
;; (def data-maps (matrix-to-maps (rest data)))
; each cell contains a list of items,
; each item is a map with Name, Quantity, Price
; cell example: ({Nombre Libreta, Cantidad 100, Precio 12} {Nombre Cuaderno, Cantidad 50, Precio 4} {Nombre Carpeta, Cantidad 100, Precio 6})
; make each cell instead of a list of items, a map of maps. 
; cell example: ({Libreta {Nombre Libreta, Cantidad 100, Precio 12} Cuaderno {Nombre Cuaderno, Cantidad 50, Precio 4} Carpeta {Nombre Carpeta, Cantidad 100, Precio 6}})
(defn cell-to-map-of-maps [cell]
  (zipmap (map #(get % 'Nombre) cell) cell))

(defn row-to-map-of-maps [row]
  (map cell-to-map-of-maps row))

(defn matrix-to-map-of-maps [matrix]
    (map row-to-map-of-maps matrix))

;; (def maps (matrix-to-map-of-maps data-maps))
;(println maps)

(defn des-imbrincar-lista [lista]
  (map #(apply merge %) lista))

;; (println " ")
;; (println (count (apply concat maps)))
;; (println " ")
;; (println (apply concat maps))
;; (println " ")

(defn des-imbrincar [lista]
  (apply concat lista))

(defn count-to-cord [count cols rows]
    [(mod count (Integer/parseInt cols)) (int (/ count (Integer/parseInt cols)))])

; create a hashmap that refers to each cell by its coordinates
; example: (0 0) -> ({Libreta {Nombre Libreta, Cantidad 100, Precio 12} Cuaderno {Nombre Cuaderno, Cantidad 50, Precio 4} Carpeta {Nombre Carpeta, Cantidad 100, Precio 6}})
(defn associate-coordinates [matrix cols rows]
    (zipmap (map #(count-to-cord % cols rows) (range (count matrix))) matrix))

;(def almacen (associate-coordinates (des-imbrincar maps)))

;(println (associate-coordinates (des-imbrincar maps)))

(defn almacen-from-csv [filename cols rows]
    (let [
        mat-maps (matrix-to-maps (rest (read-csv filename)))
        maps (matrix-to-map-of-maps mat-maps)
        unimbrincado (des-imbrincar maps)
    ]
        (associate-coordinates unimbrincado cols rows)
    ))

;(println "Almacen:")
;; (def almacen (almacen-from-csv "data.csv"))
;; (println almacen)

;; (def state-map (hash-map 'estado [0 0] 'almacen almacen))
;; (println state-map)

(defn state-map-from-csv [filename cols rows]
    (hash-map 'estado [0 0] 'almacen (almacen-from-csv filename cols rows)))

;(println (state-map-from-csv "data.csv"))

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

(defn show-actual-cell [state-map]
    (get (get state-map 'almacen) (get state-map 'estado)))

;; (def state-map (state-map-from-csv "data.csv" cols rows))
;(println state-map)
;; (println (show-actual-cell state-map))
;; (println (show-actual-cell (move state-map "right")))

(defn add-product [state-map name quantity]
    (let [almacen (get state-map 'almacen)
          estado (get state-map 'estado)
          actual-cell (get almacen estado)
          product (get actual-cell name)
          x (do
              (println actual-cell)
              (println name)
              (println quantity)
              (println (get actual-cell name))
              name)
          actual-quantity (if (= (type (get product 'Cantidad)) java.lang.Long) (get product 'Cantidad) (Integer/parseInt (get product 'Cantidad)))
          new-product (assoc product 'Cantidad (+ actual-quantity quantity))
          new-cell (assoc actual-cell name new-product)]
        (assoc state-map 'almacen (assoc almacen estado new-cell))))

(defn remove-product [state-map name quantity]
    (let [almacen (get state-map 'almacen)
          estado (get state-map 'estado)
          actual-cell (get almacen estado)
          product (get actual-cell name)
          x (do
              (println actual-cell)
              (println name)
              (println quantity)
              (println (get actual-cell name))
              name)
          actual-quantity (if (= (type (get product 'Cantidad)) java.lang.Long) (get product 'Cantidad) (Integer/parseInt (get product 'Cantidad)))
          new-product (assoc product 'Cantidad (- actual-quantity quantity))
          new-cell (assoc actual-cell name new-product)]
        (assoc state-map 'almacen (assoc almacen estado new-cell))))

;; (defn remove-product [state-map name quantity]
;;     (let [almacen (get state-map 'almacen)
;;           estado (get state-map 'estado)
;;           actual-cell (get almacen estado)
;;           product (get actual-cell name)
;;           actual-quantity (if (= (type (get product 'Cantidad)) java.lang.Number) (get product 'Cantidad) (Integer/parseInt (get product 'Cantidad)))
;;           new-product (assoc product 'Cantidad (- actual-quantity quantity))
;;           new-cell (assoc actual-cell name new-product)]
;;         (assoc state-map 'almacen (assoc almacen estado new-cell))))

;; (println (show-actual-cell state-map))
;; (println )
;; (println (show-actual-cell (add-product state-map "Martillo" 10)))
;; (println )
;; (println (show-actual-cell (remove-product state-map "Martillo" 3)))

(defn exec-query [state-map query rows cols]
    (let [query-list (clojure.string/split query #" ")
          command (first query-list)]
          (case command
            "move" (do 
                    (println "Move")
                    (println (first (rest query-list)))
                    (move state-map (first (rest query-list)) rows cols))
            "add" (do 
                    (println "Add")
                    (println (first (rest query-list)))
                    (println (first (rest (rest query-list))))
                    (add-product state-map (first (rest query-list)) (Integer/parseInt (first (rest (rest query-list))))))
            "remove" (do
                    (println "Remove")
                    (println (first (rest query-list)))
                    (println (first (rest (rest query-list))))
                    (remove-product state-map (first (rest  query-list)) (Integer/parseInt (first (rest (rest query-list))))))
            "show" (do 
                    (println "Show")
                    (println (show-actual-cell state-map))
                          state-map))
            ))

(defn exec-queries [state-map queries rows cols]
    (if (empty? queries)
        state-map
        (exec-queries (exec-query state-map (first queries) rows cols) (rest queries) rows cols))
    )

(defn item-to-list [item]
    (list (get item 'Nombre) (get item 'Cantidad) (get item 'Precio)))

(defn itemlist-to-string [itemlist]
    (str/join "." itemlist))

(defn cell-to-string [cell]
    (let [keys (keys cell)
          values (vals cell)
          items (map item-to-list values)
          items-string (map itemlist-to-string items)]
        (str/join ";" items-string)))

(defn write-row-to-list [almacen row-num rows cols]
    (let [starting-cell (* row-num (Integer/parseInt cols))]
        (map #(cell-to-string (get almacen (count-to-cord (+ starting-cell %) cols rows))) (range (Integer/parseInt cols)))))

(defn write-almacen-to-list [almacen almacen-rows almacen-cols]
    (map #(write-row-to-list almacen % almacen-rows almacen-cols) (range (Integer/parseInt almacen-rows))))

(defn almacen-list-to-csv [almacen-list]
    ;(str/join "\n" (map #(str/join "," %) almacen-list))
    ;(map #(str/join "," %) almacen-list)
    (almacen-list)
)
(defn write-to-csv [filename almacen-list]
    (csv/write-csv filename almacen-list)
)

(defn valor-item [item]
    (let [cantidad (if (= (type (get item 'Cantidad)) java.lang.Long) (get item 'Cantidad) (Integer/parseInt (get item 'Cantidad)))
          precio (if (= (type (get item 'Precio)) java.lang.Long) (get item 'Precio) (Integer/parseInt (get item 'Precio)))]
        (* cantidad precio)))

(defn valor-total-celda [celda]
    (let [keys (keys celda)
          values (vals celda)
          items (map valor-item values)]
        (reduce + items)))

(defn valor-total-almacen [almacen]
    (let [
        celdas-keys (keys almacen)
        celdas-values (vals almacen)
        celdas-valores (map valor-total-celda celdas-values)]
        (reduce + celdas-valores)))

(defn inventario-item [item]
    (if (= (type (get item 'Cantidad)) java.lang.Long) (get item 'Cantidad) (Integer/parseInt (get item 'Cantidad)))
)

(defn inventario-celda [celda]
    (let [keys (keys celda)
          values (vals celda)
          items (map inventario-item values)]
        (reduce + items)))

(defn inventario-almacen [almacen]
    (let [
        celdas-keys (keys almacen)
        celdas-values (vals almacen)
        celdas-valores (map inventario-celda celdas-values)]
        (reduce + celdas-valores)))

(defn items-faltantes [celda]
    (let [keys (keys celda)
          values (vals celda)
          items (filter #(< (if (= (type (get % 'Cantidad)) java.lang.Long) (get % 'Cantidad) (Integer/parseInt (get % 'Cantidad))) 5) values)]
        (map #(get % 'Nombre) items)))

(defn items-faltantes-almacen [almacen]
    (let [
        celdas-keys (keys almacen)
        celdas-values (vals almacen)
        celdas-valores (map items-faltantes celdas-values)]
        (reduce concat celdas-valores)))

(defn exec-process [filename output-filename]
    (let [cols (first (first (read-csv filename)))
          rows (first (rest (first (read-csv filename))))
          state-map (state-map-from-csv filename cols rows)
          queries (str/split (first (rest (rest (first (read-csv filename))))) #";")
          final-state (exec-queries state-map queries rows cols)
          valor-total (valor-total-almacen (get final-state 'almacen))
          inventario-total (inventario-almacen (get final-state 'almacen))
          inventario-faltantes (items-faltantes-almacen (get final-state 'almacen))
          DEBUG (do 
                (println "Valor total: ")
                (println valor-total)
                (println "Inventario total: ")
                (println inventario-total)
                (println "Inventario faltantes: ")
                (println inventario-faltantes)
            1)]
            ;productos con stock menor a 5
            ; iterar por cada celda y por cada item en la celda
            ; iterar por cada celda -> range de 0 a (cols * rows) convirtiendo a coordenadas
            ; iterar por cada item en la celda -> map en keys y obtener cantidad
            ; si cantidad < 5 agregar a lista nombre 
          
        ;; (write-csv (almacen-list-to-csv
        ;;     (write-almacen-to-list (get final-state 'almacen) rows cols)) output-filename)))
        ;Aqui hacer un do pa sacar lo q se ocupa
        (write-csv (write-almacen-to-list (get final-state 'almacen) rows cols) output-filename)))
;(println (first (rest (rest (first (read-csv "data.csv"))))))
(exec-process "data.csv" "output3.csv")
;(println (write-almacen-to-list (get state-map 'almacen) rows cols))