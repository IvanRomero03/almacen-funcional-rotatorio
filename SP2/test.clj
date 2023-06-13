(ns main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(def to-be-csv-writen '(["a" "b" "c"] ["d" "0" "f"] ["g" "h" "i"]))

(defn write-csv [data filename]
  (with-open [w (io/writer filename)]
    (csv/write-csv w data)))

(write-csv to-be-csv-writen "test.csv")