#lang racket
#|

Instituto Tecnologico de Costa Rica
Escuela de ingenieria en Computacion
Area de Ingenieria en Computadores
Lenguajes,Compiladores e Interpretes (CE-3104)
Primer Semestre 2019
Taller Funcional
Estudiante: Oscar Gonzalez Alfaro (2017121525)

|#


#|

Ejercicio 2.1
Funcion Factorial
Entrada: numero
Salida: Factorial del numero

|#

(define (fact num )
  (cond ((zero? num) 1)
        (else (* num (fact (- num 1)))
              )
        )
  )

#|

Ejercicio 2.2
Funcion Fibonacci
Entrada: numero
Salida: Fibonacci del numero

|#

(define (fib num)
  (cond ((zero? num) 1)
        ((equal? 1 num) 1)
        (else(+ (fib(- num 1)) (fib(- num 2)))
             )
        )
  )

#|

Ejercicio 2.3
Funcion Miembro
Entrada: numero, lista o string a buscar y lista donde se pretende buscar
Salida: #t si es miembro, #f si no lo es.

|#

(define (miembro elemento lista)
  (cond ((null? lista) #f)
        ((equal? elemento (car lista)) #t)
        (else (miembro elemento (cdr lista)))
        )
  )

#|

Ejercicio 2.4
Funcion Eliminar
Entrada: elemento seleccionado para eliminarlo y la lista en donde se encuentra el elemento
Salida: la lista sin el elemento seleccionado

|#

(define (eliminar_list elemento lista)
  (cond ((null? lista) null)
        ((equal? elemento (car lista)) (eliminar_list elemento (cdr lista)))
        (else (cons (car lista) (eliminar_list elemento (cdr lista))))
        )
  )

#|

Ejercicio 2.5
Funcion Quicksort
Entrada: lista 
Salida: lista ordenada

|#

;Funcion pivote
(define (pivot lista)
  (cond ((null? lista) #f)
        (else (pivot_aux (car lista) (cdr lista) '() '() ))
        )
  )

;Funcion auxiliar de pivote
(define (pivot_aux punto lista menores mayores)
  (cond ((null? lista) (list menores mayores))
        ((<= (car lista) punto) (pivot_aux punto (cdr lista) (cons (car lista) menores) mayores))
        (else (pivot_aux punto (cdr lista) menores (cons (car lista) mayores)))
        )
  )

;Funcion principal Quicksort (Principal)
(define (quicksort lista)
  (cond ((null? lista) '())
        (else (append (quicksort (car (pivot lista))) (list (car lista)) (quicksort (cadr (pivot lista)))))
        )
  )


#|

Ejercicio 2.6
Funcion Emparejamiento
Entrada: Lista de simbolos que representan los atributos y una lista de simbolos con los valores de los atributos
Salida: Una lista que contiene los pares, cada par contiene su atributo y su valor

|#

;Funcion Ejercicio (Principal) 
(define (ejercicio atributos valores)
                    (cond((null? atributos) valores)
                         ((null? valores) atributos)
                         ((not(equal? (length valores) (length atributos))) (write "Las listas deben ser de igual magnitud"))
                         (else( ejercicio_aux atributos valores '())
                              )
                         )
  )

;Funcion auxiliar de ejercicio
(define (ejercicio_aux atributos valores final)
                         (cond((null? atributos)final)
                              (else(ejercicio_aux (cdr atributos) (cdr valores) (append final (list (list (car atributos) (car valores))))))
                              )
   )

#|

Ejercicio 2.7
Funcion Eliminar un elemento de un arbol binario
Entrada: elemento a eliminar y el arbol donde se encuentra
Salida: el arbol sin el elemento indicado

|#

;Funcion Arbol
(define (arbol raiz hijo_izq hijo_der)
  (cond ((and (null? hijo_izq) (null? hijo_der)) raiz)
        (else (list raiz hijo_izq hijo_der))
        )
  )

;Funcion Raiz
(define (raiz arbol)
  (cond ((null? arbol) arbol)
        ((and (null? (hijo_izq arbol)) (null? (hijo_der arbol))) arbol)
        (else (car arbol))
        )
  )

;Funcion Hijo Izquierdo
(define (hijo_izq arbol)
  (cond ((hoja arbol)'() )
        (else (cadr arbol))
        )
  )
         
;Funcion Hijo derecho
(define (hijo_der arbol)
  (cond ((hoja arbol)'() )
        (else (caddr arbol))
        )
  )

;Funcion Hoja
(define (hoja arbol)
  (not (list? arbol)))

;Funcion Mayor
(define (mayor arbol)
  (cond ((null? arbol) '() )
        ((null? (hijo_der arbol)) (raiz arbol))
        (else (mayor (hijo_der arbol)))
        )
  )

;Funcion Eliminar (Principal)
(define (eliminar elemento arb)
  (cond ((null? arb) '())
        ((< elemento (raiz arb)) (arbol (raiz arb) (eliminar elemento (hijo_izq arb)) (hijo_der arb)))
        ((> elemento (raiz arb)) (arbol (raiz arb) (hijo_izq arb) (eliminar elemento (hijo_der arb))))
        
        ;nodo sin hijos
        ((and (null? (hijo_izq arb))(null? (hijo_der arb)))'() )

        ;nodo sin hijo izquierdo
        ((null? (hijo_izq arb)) (hijo_der arb))

        ;nodo sin hijo derecho
        ((null? (hijo_der arb)) (hijo_izq arb))

        ;nodo tiene 2 hijos
        (else (arbol (mayor (hijo_izq arb)) (eliminar (mayor (hijo_izq arb))(hijo_izq arb))(hijo_der arb)))
        )
  )
        
#|

Ejercicio 2.8
Funcion Encontrar las rutas de anchura primero
Entrada: inicio, fin y la representacion del grafo
Salida: Rutas de anchura

|#

;Funcion representacion del grafo
(define representacion '( (i (a b))
              (a (i c d))
              (b (i c d))
              (c (a b x))
              (d (a b f))
              (x (c))
              (f (d))
              ))
              
;Funcion que obtiene una solucion cuando se llego al nodo destino
(define (solucion fin camino)
  (equal? fin (car camino)))

;Funcion que retorna los vecinos inmediatos
(define (vecinos elemento grafo)
  (let ((resultado (assoc elemento grafo)))
    (cond ((equal? resultado #f) #f)
          (else (cadr resultado))
          ))
  )

;Funcion que crea nuevas trayectorias
(define (extender camino grafo)
  (apply append (map (lambda(x)
                       (cond ((miembro x camino) '())
                             (else (list (cons x camino)))))
                     (vecinos (car camino) grafo))))

;Funcion anchura (Principal)
(define (anchura ini fin grafo)
  (anchura_aux (list (list ini)) fin grafo '()))

;Funcion auxiliar anchura
(define (anchura_aux rutas fin grafo soluciones)
  (cond ((null? rutas) (map reverse soluciones))
        ((solucion fin (car rutas)) (anchura_aux (cdr rutas) fin grafo (cons (car rutas) soluciones)))
        (else (anchura_aux (append (cdr rutas) (extender (car rutas) grafo)) fin grafo soluciones))
        )
  )
