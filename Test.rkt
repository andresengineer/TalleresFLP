;; Jean Paul Davalos Valencia
;; 1832375
;;
;; Wilson Andrés Mosquera Zapata
;; 2182116

#lang eopl

(require "Clase3.rkt")
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
(display "Pruebas con Listas:")
(newline)
(display graph-list-01)
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

(display "Pruebas con datatypes:")
(newline)
;;(display graph-datatype-01)
(newline)

;; ===================================================
;; Pruebas para PARSEBNF:
;; ===================================================

(define parseBNF-01
  (PARSEBNF graph-list-01))

(display "Pruebas para PARSEBNF:")
(newline)
;;(display parseBNF-01)
(newline)

;; ===================================================
;; Pruebas para PARSEBNF:
;; ===================================================

(define unparseBNF-01
  (UNPARSEBNF graph-datatype-01))

(display "Pruebas para UNPARSEBNF:")
(newline)
;;(display unparseBNF-01)
(newline)

;; ===================================================
;; Pruebas para add-edge:
;; ===================================================
#|
(display "Grafo sin añadir arista:")
(newline)
(display graph-list-01)
(newline)
(display "Grafo con arista añadida:")
(newline)
(define edge-added-01 (add-edge graph-list-01 '(d c)))
(display edge-added-01)
(newline)
|#

;; ===================================================
;; Pruebas para vecinos-salientes:
;; ===================================================

(display "Grafo original:")
(display graph-datatype-01)
(newline)
(newline)

(display "Vecinos salientes de 'c': ")
(display (vecinos-salientes graph-datatype-01 'c))
(newline)
(newline)


;; ===================================================
;; Pruebas para vecinos-salientes:
;; ===================================================

(display "Grafo original:")
(display graph-datatype-01)
(newline)
(newline)

(display "Vecinos entrantes de 'c': ")
(display (vecinos-entrantes graph-datatype-01 'c))
(newline)