;; Jean Paul Davalos Valencia
;; 1832375
;;
;; Wilson Andrés Mosquera Zapata
;; 2182116

#lang eopl

(require "ejercicio1.rkt")
(provide (all-defined-out))

;; ===================================================
;; Pruebas con listas:
;; ===================================================


(define graph-list-01
  (graph-cons
   (vertice-cons '(a b c d))
   (edge-cons '((a b) (c d) (c b) (a c))))
  )

(newline)
(display "Grafo 1 (Listas):")
(newline)
(display graph-list-01)
(newline)
(newline)

;-----------------------------------------------------

(define graph-list-02
  (graph-cons
   (vertice-cons '(a b c d e f))
   (edge-cons '((a f) (c f) (c a) (d b) (e d) (d e) (b e) (b c))))
  )

(display "Grafo 2 (Listas):")
(newline)
(display graph-list-02)
(newline)
(newline)

;-----------------------------------------------------

(define graph-list-03
  (graph-cons
   (vertice-cons '(x y z w))
   (edge-cons '((x y) (y z) (z w) (w x) (y w))))
  )

(display "Grafo 3 (Listas):")
(newline)
(display graph-list-03)
(newline)
(newline)


;; ===================================================
;; Pruebas con datatypes:
;; ===================================================

(define graph-datatype-01
  (graph
    (vertices '(a b c d))
    (edges
      (list
        (edge 'a 'b)
        (edge 'c 'd)
        (edge 'c 'b)
        (edge 'a 'c)))
    )
  )

(display "Grafo 1 (Datatype):")
(newline)
(display graph-datatype-01)
(newline)
(newline)

;-----------------------------------------------------

(define graph-datatype-02
  (graph
    (vertices '(a b c d e f))
    (edges
      (list
        (edge 'a 'f)
        (edge 'c 'f)
        (edge 'c 'a)
        (edge 'd 'b)
        (edge 'e 'd)
        (edge 'd 'e)
        (edge 'b 'e)
        (edge 'b 'c)))
    )
  )

(display "Grafo 2 (Datatype):")
(newline)
(display graph-datatype-02)
(newline)
(newline)

;-----------------------------------------------------

(define graph-datatype-03
  (graph
    (vertices '(x y z w))
    (edges
      (list
        (edge 'x 'y)
        (edge 'y 'z)
        (edge 'z 'w)
        (edge 'w 'x)
        (edge 'y 'w)))
    )
  )

(display "Grafo 3 (Datatype):")
(newline)
(display graph-datatype-03)
(newline)
(newline)

;; ===================================================
;; Pruebas para PARSEBNF:
;; ===================================================

(define parseBNF-01
  (PARSEBNF graph-list-01))

(display "PARSEBNF al Grafo 1 (Listas):")
(newline)
(display parseBNF-01)
(newline)
(newline)

;-----------------------------------------------------

(define parseBNF-02
  (PARSEBNF graph-list-02))

(display "PARSEBNF al Grafo 2 (Listas):")
(newline)
(display parseBNF-02)
(newline)
(newline)

;-----------------------------------------------------

(define parseBNF-03
  (PARSEBNF graph-list-03))

(display "PARSEBNF al Grafo 3 (Listas):")
(newline)
(display parseBNF-03)
(newline)
(newline)


;; ===================================================
;; Pruebas para UNPARSEBNF:
;; ===================================================

(define unparseBNF-01
  (UNPARSEBNF graph-datatype-01))

(display "UNPARSEBNF al Grafo 1 (Datatype):")
(newline)
(display unparseBNF-01)
(newline)
(newline)

;-----------------------------------------------------

(define unparseBNF-02
  (UNPARSEBNF graph-datatype-02))

(display "UNPARSEBNF al Grafo 2 (Datatype):")
(newline)
(display unparseBNF-02)
(newline)
(newline)

;-----------------------------------------------------

(define unparseBNF-03
  (UNPARSEBNF graph-datatype-03))

(display "UNPARSEBNF al Grafo 3 (Datatype):")
(newline)
(display unparseBNF-03)
(newline)
(newline)


;; ===================================================
;; Pruebas para add-edge:
;; ===================================================

(display "Grafo 1 (Listas) sin añadir arista:")
(newline)
(display graph-list-01)
(newline)
(display "Grafo 1 (Listas) con arista añadida:")
(newline)
(define edge-added-01 (add-edge graph-list-01 '(a d)))
(display edge-added-01)
(newline)
(newline)

;-----------------------------------------------------

(display "Grafo 2 (Listas) sin añadir arista:")
(newline)
(display graph-list-02)
(newline)
(display "Grafo 2 (Listas) con arista añadida:")
(newline)
(define edge-added-02 (add-edge graph-list-02 '(a f)))
(display edge-added-02)
(newline)
(newline)

;-----------------------------------------------------

(display "Grafo 3 (Listas) sin añadir arista:")
(newline)
(display graph-list-03)
(newline)
(display "Grafo 3 (Listas) con arista añadida:")
(newline)
(define edge-added-03 (add-edge graph-list-03 '(z y)))
(display edge-added-03)
(newline)
(newline)


;; ===================================================
;; Pruebas para vecinos-salientes:
;; ===================================================

(display "Grafo 1 (Datatype):")
(display graph-datatype-01)
(newline)
(newline)

(display "Vecinos salientes de 'c': ")
(display (vecinos-salientes graph-datatype-01 'c))
(newline)
(newline)

;-----------------------------------------------------

(display "Grafo 2 (Datatype):")
(display graph-datatype-02)
(newline)
(newline)

(display "Vecinos salientes de 'a': ")
(display (vecinos-salientes graph-datatype-02 'a))
(newline)
(newline)

;-----------------------------------------------------

(display "Grafo 3 (Datatype):")
(display graph-datatype-03)
(newline)
(newline)

(display "Vecinos salientes de 'y': ")
(display (vecinos-salientes graph-datatype-03 'y))
(newline)
(newline)


;; ===================================================
;; Pruebas para vecinos-entrantes:
;; ===================================================

(display "Grafo original:")
(display graph-datatype-01)
(newline)
(newline)

(display "Vecinos entrantes de 'c': ")
(display (vecinos-entrantes graph-datatype-01 'c))
(newline)
(newline)

;-----------------------------------------------------

(display "Grafo 2 (Datatype):")
(display graph-datatype-02)
(newline)
(newline)

(display "Vecinos entrantes de 'a': ")
(display (vecinos-entrantes graph-datatype-02 'a))
(newline)
(newline)

;-----------------------------------------------------

(display "Grafo 3 (Datatype):")
(display graph-datatype-03)
(newline)
(newline)

(display "Vecinos entrantes de 'y': ")
(display (vecinos-entrantes graph-datatype-03 'y))
(newline)
(newline)