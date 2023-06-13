; 1.- Una lista de con los valores enteros de n a -n, donde n es cualquier valor grande previamente definido por ti.

(defn lista-n [n]
  (range n (- (+ n 1)) -1))

;(println (lista-n 10))

; 2.- Una lista con los primeros 10,000 números enteros impares a partir del 5263.
(defn lista-impares []
  (range 5263 (+ 5263 20000) 2))

;(println (lista-impares))

; 3.- Una lista que repita n veces un número aleatorio de puntos flotante entre 0 y 1, donde n es cualquier valor grande previamente definido por ti.
(defn lista-rand [n]
  (repeat n (rand)))

;(println (lista-rand 10))

; 4.- Una lista con 5,000 números aleatorios enteros positivos menores a 1000.
(defn lista-rand-int [n]
  (take n (repeatedly #(rand-int 1000))))

;(println (lista-rand-int 5000))

; 5.- Una lista que contenga el valor obtenido al tirar n veces dos dados tradicionales (valores del 1 al 6), donde n es cualquier valor grande previamente definido por ti.
(defn dado []
  (+ 1 (rand-int 6)))

(defn lista-dados [n]
  (take n (repeatedly #(+ (dado) (dado)))))

;(println (lista-dados 10))

; 6.- Implementa en Clojure el algoritmo del Quicksort equivalente al que implementaste en un entrenamiento previo.
(defn quicksort [lista]
  (if (empty? lista)
    lista
    (let [pivot (first lista) ; Tomar el primer elemento como pivote
          left (filter #(<= % pivot) (rest lista)) ; Lista de elementos menores o iguales al pivote
          right (filter #(> % pivot) (rest lista))] ; Lista de elementos mayores al pivote
      (concat (quicksort left) (list pivot) (quicksort right)))))

;En este algoritmo, es posible paralelizar la ejecucion de las llamadas recursivas a quicksort de este modo, creando un arbol recursivo paralelizable en si

;(println (time (dorun (quicksort (lista-rand-int 100000)))))
; "Elapsed time: 3604.7797 msecs"
;(println (quicksort (lista-rand-int 100)))
(defn cont-digs [n]
  (if (< n 10)
    1
    (+ 1 (cont-digs (/ n 10)))))

;(println (cont-digs 1234567890))

(def datos (repeatedly 100000 #(rand-int 100000000)))

;(println datos)
(println (time (dorun (pmap #(dorun (map cont-digs %)) (partition-all 5000 datos)))))