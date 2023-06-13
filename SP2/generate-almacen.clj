(ns main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(def posible-names (list "Destornillador" "Martillo" "Alicate" "Serrucho" "Tornillo" "Clavo" "Cable" "Tuerca" "Arduino" "Raspberry" "OrangePi" "IntelGalileo" "Boligrafo" "Goma" "Sacapuntas" "Tijeras" "Pegamento" "Cuaderno" "Carpeta" "Lapiz" "JetsonNano"))
(def max-item-quantity 50)
(def max-item-price 1000)
(def max-cell-items 10)
(def max-rows 10)
(def max-columns 10)

(defn generate-item []
  (let [name (nth posible-names (rand-int (count posible-names)))
        quantity (rand-int max-item-quantity)
        price (rand-int max-item-price)]
        ;{Nombre BeagleBone, Cantidad 2, Precio 150},
    {'Nombre name 'Cantidad quantity 'Precio price}))

(defn generate-cell []
    (let [items-quantity (rand-int max-cell-items)
          cell-hash (hash-map)
            items (for [i (range items-quantity)]
                    (let [item (generate-item)]
                        (assoc cell-hash (get item 'Nombre) item)))]
        items))

(println (generate-cell))
