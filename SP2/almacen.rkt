#lang racket

; infinite caroussel of a matrix of 4x3 with lists init
(define almacen-test
    '('('(1 2 3) '(4 5 6) '(7 8 9)) '('(10 11 12) '(13 14 15) '(16 17 18)) '('(19 20 21) '(22 23 24) '(25 26 27)) '('(28 29 30) '(31 32 33) '(34 35 36)))
)

; actions: up, down, left, right
; test actions up up down right right down left

(define (move-matrix action almacen)
    (cond
        [(equal? action 'up) (move-up almacen)]
        [(equal? action 'down) (move-down almacen)]
        [(equal? action 'left) (move-left almacen)]
        [(equal? action 'right) (move-right almacen)]
    )
)

(define (get-last list)
    (cond
        [(null? (cdr list)) (car list)]
        [else (get-last (cdr list))]
    )
)

(define (move-up almacen)
    (cond
        [(null? (cdr almacen)) almacen]
        [else (cons (get-last (car almacen)) (move-up (cdr almacen)))]
    )
)   

(define (move-down almacen)
    (cond
        [(null? (cdr almacen)) almacen]
        [else (cons (car almacen) (move-down (cdr almacen)))]
    )
)

(define (move-left almacen)
    (cond
        [(null? (cdr almacen)) almacen]
        [else (cons (map car almacen) (move-left (map cdr almacen)))]
    )
)

(define (move-right almacen)
    (cond
        [(null? (cdr almacen)) almacen]
        [else (cons (map cdr almacen) (move-right (map car almacen)))]
    )
)

(define (print-matrix almacen)
    (cond
        [(null? almacen) (newline)]
        [else (begin (display (car almacen)) (newline) (print-matrix (cdr almacen)))]
    )
)

(define test-actions '(up up down right right down left))

(define (test-actions-fn almacen actions)
    (cond
        [(null? actions) (print-matrix almacen)]
        [else (begin (print-matrix almacen) (newline) (test-actions-fn (move-matrix (car actions) almacen) (cdr actions)))]
    )
)

(test-actions-fn almacen-test test-actions)
;(if (equal? (car test-actions) 'up) 'asd 'no)