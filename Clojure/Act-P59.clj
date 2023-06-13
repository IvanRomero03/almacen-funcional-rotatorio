; Lógica de es primo: Iterar por los números hasta 2 (primer primo) y ver si es divisible por alguno de ellos. Si no lo es, es primo.
(defn isPrime? [n]
  (if (> (apply + (map #(if (= (mod n %) 0) 1 0) (range 2 (+ 1 (Math/ceil (Math/sqrt n)))))) 0) false true))

(defn getPrimes [n]
  (filter isPrime? (range 2 n)))

(defn getSumaPrimos [n]
  (apply + (getPrimes n)))

;(println (time (getSumaPrimos 5000000)))
; "Elapsed time: 1619106.502 msecs"
; 838596693108

(defn isPrimeParalelizied? [n]
  (if (> (apply + (pmap #(if (= (mod n %) 0) 1 0) (range 2 (+ 1 (Math/ceil (Math/sqrt n)))))) 0) false true))

(defn getPrimesParalelizied [n]
  (pmap #(if (isPrimeParalelizied? %) % 0) (range 2 n)))

(defn getSumaPrimosParalelizied [n]
  (apply + (getPrimesParalelizied n)))

(println (time (getSumaPrimosParalelizied 5000000)))
; "Elapsed time: 20273.612 msecs"
; 838596693108