;; Jean Paul Davalos Valencia
;; 1832375
;;
;; Wilson Andrés Mosquera Zapata
;; 2182116

#lang eopl

(provide (all-defined-out))


;; ===================================================
;; 2.1.1 Gramática BNF (Basada en Listas).
;; ===================================================


;<graph> ::= (<graph> <nodes> <edges>)

; <nodes> ::= (<nodes> <nodelist>)

; <edges> ::= (<edges> <edgelist>)

;; Constructores:

(define vertice-cons
  (lambda (vertexlist)
    (list 'vertices vertexlist)
    )
  )

(define edge-cons
  (lambda (edgelist)
    (list 'edges edgelist)
    )
  )

(define graph-cons
  (lambda (vertices edges)
    (list 'graph vertices edges)
    )
  )

;; Extrae los vértices de un grafo
(define graph->vertices
  (lambda (graph)
    (cadr graph)
    )
  )

;; Extrae las aristas de un grafo
(define graph->edges
  (lambda (graph)
    (caddr graph)
    )
  )

;; Extrae la lista de vértices de los vértices
(define vertices->vertexlist
  (lambda (vertices)
    (cadr vertices)
    )
  )

;; Extrae la lista de aristas de las aristas
(define edges->edgelist
  (lambda (edges)
    (cadr edges)
    )
  )



;; ===================================================
;; 2.1.2 Gramática BNF (Datatypes).
;; ===================================================

;; Proposito: Definir grafos dirigidos con basandonos en datatypes.
;; Gramatica:
;; gragh ::= (<graph> vertices edges)
;; vertices ::= (<vertices> <list>)
;; edges ::= (<edges> edge)
;; edge ::= (<element> <element>)


;; Define el datatype para una arista
(define-datatype edge-exp edge-exp?
  (edge (start symbol?) (end symbol?)))

;; Define el datatype para los vértices
(define-datatype vertices-exp vertices-exp?
  (vertices (nodelist (list-of symbol?))))

;; Define el datatype para las aristas
(define-datatype edges-exp edges-exp?
  (edges (edgelist (list-of edge-exp?))))

;; Define el datatype para un grafo
(define-datatype graph-exp graph-exp?
  (graph (vertices vertices-exp?)
         (edges edges-exp?)))


;; ===================================================
;; 2.2.1 Función PARSE.
;; ===================================================

(define PARSEBNF
  (lambda (exp)
    (cond
      [(eqv? (car exp) 'graph) (graph (PARSEBNF (cadr exp)) (PARSEBNF (caddr exp)))]
      [(eqv? (car exp) 'vertices) (vertices (cadr exp))]
      [(eqv? (car exp) 'edges) (edges (PARSEBNF (cons 'edge (cadr exp))))]
      [(eqv? (car exp) 'edge) (parse-edges-list (cdr exp))]
      )
    )
  )

;; Función auxiliar para convertir una lista de aristas.
(define parse-edges-list
  (lambda (edgelist)
    (if (null? edgelist)
        '()
        (cons (edge (caar edgelist) (cadar edgelist)) (parse-edges-list (cdr edgelist)))
        )
    )
  )


;; ===================================================
;; 2.2.2 Función UNPARSE.
;; ===================================================

;; Función UNPARSEBNF
(define UNPARSEBNF
  (lambda (exp)
    (cases graph-exp exp
      (graph (vertices edges)
             (list 'graph (unparse-vertices vertices) (unparse-edges edges))))
    )
  )


;; Función auxiliar para los vértices.
(define unparse-vertices
  (lambda (exp)
    (cases vertices-exp exp
      (vertices (nodelist)
                (list 'vertices nodelist))
    )
  )
)

;; Función auxiliar para las aristas.
(define unparse-edges
  (lambda (exp)
    (cases edges-exp exp
      (edges (edgelist)
             (cons 'edges (unparse-edges-list edgelist)))
    )
  )
)

;; Función auxiliar para una arista.
(define unparse-edge
  (lambda (exp)
    (cases edge-exp exp
      (edge (a b)
            (list a b)))
  )
)

;; Función auxiliar para convertir una lista de aristas.
(define unparse-edges-list
  (lambda (edgelist)
    (if (null? edgelist)
        empty
        (cons (unparse-edge (car edgelist)) (unparse-edges-list (cdr edgelist))))
  )
)

;; ===================================================
;; 2.3.1 Adicionar una nueva arista.
;; ===================================================

(define add-edge
  (lambda (graph1 edge-to-add)
    (letrec
      ([aristas (edges->edgelist (graph->edges graph1))])
      (cond
        [(null? edge-to-add) graph1]
        [(equal? #t (edge-exist? aristas edge-to-add)) graph1]
        [else (graph-cons (graph->vertices graph1) 
                          (edge-cons (merge-lists aristas (list edge-to-add))))]))))

;; Función auxiliar para verificar si existe una arista en una lista.
(define edge-exist?
  (lambda (graph1 edge-to-add)
    (and (pair? graph1) 
         (or (equal? (car graph1) edge-to-add) 
             (edge-exist? (cdr graph1) edge-to-add)))))

;; Función auxiliar para  para combinar dos listas.
(define merge-lists
  (lambda (L1 L2)
    (if (pair? L1)
        (cons (car L1) (merge-lists (cdr L1) L2))
        L2)))


;; ===================================================
;; Funciones Globales para el manejo de vecinos
;; ===================================================

;; Encuentra los vecinos de un vértice dado en un grafo según un predicado.
(define (vecinos grafo vertice predicado-vecino)
  (cases graph-exp grafo
    (graph (vertices lista-aristas)
      (eliminar-duplicados (buscar-vecinos-en-edges lista-aristas vertice predicado-vecino)))
  )
)

;; Busca los vecinos en una lista de aristas.
(define (buscar-vecinos-en-edges lista-aristas vertice predicado-vecino)
  (cases edges-exp lista-aristas
    (edges (aristas)
      (buscar-vecinos-en-aristas aristas vertice predicado-vecino))
  )
)

;; Función auxiliar para buscar vecinos en una lista de aristas.
(define (buscar-vecinos-en-aristas aristas vertice predicado-vecino)
  (cond
    [(null? aristas) '()]
    [else (append (buscar-vecinos-en-edge (car aristas) vertice predicado-vecino)
                  (buscar-vecinos-en-aristas (cdr aristas) vertice predicado-vecino))]))

;; Encuentra los vecinos en una arista específica usando un predicado.
(define (buscar-vecinos-en-edge arista vertice predicado-vecino)
  (cases edge-exp arista
    (edge (inicio fin)
      (if (predicado-vecino inicio fin vertice)
          (list (predicado-vecino inicio fin vertice))
          '())
    )
  )
)

;; Elimina duplicados de una lista.
(define (eliminar-duplicados lista)
  (define (aux lista resultado)
    (cond
      [(null? lista) (reverse resultado)]
      [(member (car lista) resultado) (aux (cdr lista) resultado)]
      [else (aux (cdr lista) (cons (car lista) resultado))]))
  (aux lista '()))

;; ===================================================
;; 2.3.2 Vecinos salientes.
;; ===================================================

;; Encuentra los vecinos salientes de un vértice dado en un grafo.
(define (vecinos-salientes grafo vertice)
  (vecinos grafo vertice (lambda (start end v)
                           (and (equal? start v) end))))

;; ===================================================
;; 2.3.3 Vecinos entrantes.
;; ===================================================

;; Encuentra los vecinos entrantes de un vértice dado en un grafo.
(define (vecinos-entrantes grafo vertice)
  (vecinos grafo vertice (lambda (start end v)
                           (and (equal? end v) start))))

