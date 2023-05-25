#lang racket
(require csv-reading)
(require racket/dict)
(require csv-writing)
; Read CSV
(define (read-csv file)
    (csv->list (make-csv-reader (open-input-file file)))
)  
; define the data to be used
(define data (read-csv "data.csv"))
(define cols (caar data))
(define rows (cadar data))

; Convert list of lists to list of strings
(define (cell-to-dict cell)
    (map (lambda (x) (string-split x ".")) (string-split cell ";")))
    
; Convert the data to the format of each cells
(define (rows-to-cells rows)
    (map (lambda (row) (map cell-to-dict row)) rows))

; Create a hash with the data
(define (create-dict data-input [dict (make-hash)] [n 1])
    (define (create-dict-helper data-input-fun dict n)
        (if (null? data-input-fun)
            dict 
            (create-dict-helper (cdr data-input-fun) (dict-set dict n (car data-input-fun)) (+ n 1))))
    (create-dict-helper data-input dict n))

;Remove product -> name quantity
(define (get-new-removed-list name quantity lista)
    (if (null? lista)
        (raise "product not found")
        (if (string-ci=? (car (car lista)) name)
            (if (>= (string->number (cadr (car lista))) quantity)
                (cons (list name (- (string->number (cadr (car lista))) quantity) (caddr (car lista))) (cdr lista))
                (raise "not enough quantity"))
            (cons (car lista) (get-new-removed-list name quantity (cdr lista))))
    )
)
; Remove product -> name quantity
(define (remove-product name quantity state-dict)
        (list (dict-set (car state-dict) (cadr state-dict) (get-new-removed-list name quantity (dict-ref (car state-dict) (cadr state-dict)))) (cadr state-dict)))


;Add product -> name quantity
(define (get-new-added-list name quantity lista)
    (if (null? lista)
        (raise "product not found")
        (if (string-ci=? (car (car lista)) name)
            (cons (list name (+ (string->number (cadr (car lista))) quantity) (caddr (car lista))) (cdr lista))
            (cons (car lista) (get-new-added-list name quantity (cdr lista))))
    )
)

; Add product -> name quantity
(define (add-product name quantity state-dict)
        (list (dict-set (car state-dict) (cadr state-dict) (get-new-added-list name quantity (dict-ref (car state-dict) (cadr state-dict)))) (cadr state-dict)))


;Move state -> direction
(define (move-direction dir state-dict)
    (cond 
        ((equal? dir 'up)
            (if (<= (cadr state-dict) (string->number cols))
                (list (car state-dict) (- (string->number rows) (string->number cols)))
                (list (car state-dict) (- (cadr state-dict) (string->number cols)))))
        ((equal? dir 'down)
                (if (> (cadr state-dict) (* (string->number rows) (- (string->number cols) 1)))
                (list (car state-dict) (+ (cadr state-dict) (- (string->number cols) (* (string->number rows) (- (string->number cols) 1)))) )
                (list (car state-dict) (+ (cadr state-dict) (string->number cols))))
        )
        ((equal? dir 'left)
            (if (equal? (modulo (cadr state-dict) (string->number cols)) 0)
                (list (car state-dict) ((string->number cols)))
                (list (car state-dict) (- (cadr state-dict) 1))))
        ((equal? dir 'right)
            (if (equal? (modulo (cadr state-dict) (string->number cols)) (- (string->number cols) 1))
                (list (car state-dict) (- (cadr state-dict) (- (string->number cols) 1)))
                (list (car state-dict) (+ (cadr state-dict) 1))))
        (else (raise "invalid direction"))
    )
)

;Show current cell
(define (show-cell state-dict)
    (begin 
        (display (dict-ref (car state-dict) (cadr state-dict)))
        (newline)
        state-dict)
)
(define simplyfy-list (lambda (list) (apply append list)))
(define dict (create-dict (simplyfy-list (rows-to-cells (cdr data))) (hash) 1))
(define state-dict-objct (list dict 1))
(define instuctions-query '((show) (move down) (show) (remove "JetsonNano" 9) (show) (move right) (show) (move up) (show) (add "Serrucho" 5) (show)))

; Execute query of instructions
(define (execute-query-helper state-dict query)
    (cond
        ((equal? (car query) 'show)
            (show-cell state-dict)
            )
        ((equal? (car query) 'move)
                (move-direction (cadr query) state-dict))
        ((equal? (car query) 'remove)
            (remove-product (cadr query) (caddr query) state-dict))
        ((equal? (car query) 'add)
            (add-product (cadr query) (caddr query) state-dict))
        (else (raise "invalid query")))
)

; Execute query of instructions
(define (execute-query state-dict query)
    (if (null? query)
        state-dict
        (execute-query (execute-query-helper state-dict (car query)) (cdr query))
        )
)

; Convert list of lists to list of strings
(define (to-full-string-list lista)
    (if (null? lista)
        '()
        (if (string? (car lista))
            (cons (car lista) (to-full-string-list (cdr lista)))
            (cons (number->string (car lista)) (to-full-string-list (cdr lista))
            ))
    )
)

(define list-to-string (lambda (lista) (string-join (to-full-string-list lista) ".")))

; Convert each cell to format to save
(define (to-string lista)
    (if (null? lista)
        ""
        (if (null? (cdr lista))
            (list-to-string (car lista))
            (string-append (list-to-string (car lista)) ";" (to-string (cdr lista))))
    )
)

; convert each cell to format to save
(define (to-list graph n)
    (if (equal? (modulo n (string->number cols)) 0)
        (string-append (to-string (dict-ref graph n)) "\n")
        (cons (to-string (dict-ref graph n)) (to-list graph (+ n 1))))
)

; convert each row to list of strings
(define (to-row graph n)
    (if (equal? (modulo n (string->number cols)) 0)
        (list (to-string (dict-ref graph n)))
        (cons (to-string (dict-ref graph n)) (to-row graph (+ n 1))))
)

; convert each row to list of cols
(define (to-matrix graph n)
    ; from 1 to 1 + cols... to row*(cols - 1)
    (if (> n (+ 1 (* (string->number rows) (- (string->number cols) 1))))
        '()
        (cons (to-row graph n) (to-matrix graph (+ n (string->number cols))))
    )
)

; write the csv from matrix
; de https://stackoverflow.com/questions/61023450/scheme-how-to-write-list-of-lists-into-csv-file
(define (write-to-a-file lst path)
  (call-with-output-file path
    (lambda (output-port)
      (display-table lst output-port))
    #:exists 'replace))

(to-matrix (car state-dict-objct) 1)

(define changed-graph (execute-query state-dict-objct instuctions-query))

(write-to-a-file (cons (list rows cols) (to-matrix (car changed-graph) 1)) "output.csv")