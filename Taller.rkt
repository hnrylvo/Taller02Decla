#lang racket

;; ============================================
;; TALLER 2 – Programación Declarativa (SOLUCIONES)
;; ============================================

;; --------------------------------------------------
;; 1. Contar elementos positivos
;; --------------------------------------------------
(define (contar-positivos lista)
  (length (filter positive? lista)))

(displayln (contar-positivos '(3 -2 7 0 -5 9)))
; => 3
(newline)


;; --------------------------------------------------
;; 2. Generar lista de cuadrados de números pares
;; --------------------------------------------------
(define (lista-cuadrados lista)
  (let* ((pares (filter even? lista))
         (cuadrados (map (lambda (x) (* x x)) pares)))
    cuadrados))

(displayln (lista-cuadrados '(1 2 3 4 5 6 7 8)))
; => '(4 16 36 64)
(newline)


;; --------------------------------------------------
;; 3. Factorial usando recursión
;; --------------------------------------------------
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln (factorial 5))
; => 120
(newline)


;; --------------------------------------------------
;; 4. Elevar cada número al cubo
;; --------------------------------------------------
(define (cubo lista)
  (map (lambda (x) (* x x x)) lista))

(displayln (cubo '(2 3 4)))
; => '(8 27 64)
(newline)


;; --------------------------------------------------
;; 5. Sumar elementos impares
;; --------------------------------------------------
(define (sumar-impares lista)
  (let* ((impares (filter odd? lista))
         (suma (foldl + 0 impares)))
    suma))

(displayln (sumar-impares '(1 2 3 4 5 6 7)))
; => 16
(newline)


;; --------------------------------------------------
;; 6. Determinar si una lista contiene negativos
;; --------------------------------------------------
(define (contiene-negativos lista)
  (ormap (lambda (x) (< x 0)) lista))

(displayln (contiene-negativos '(5 9 -3 2)))
; => #t
(newline)


;; --------------------------------------------------
;; 7. Suma acumulada de la lista
;; --------------------------------------------------
(define (suma-acumulada lista)
  (foldl
   (lambda (x acc)
     (append acc (list (+ x (last acc)))))
   '(0)
   lista))


(displayln (suma-acumulada '(1 2 3 4)))
; => '(1 3 6 10)
(newline)


;; --------------------------------------------------
;; 8. Concatenar cadenas
;; --------------------------------------------------
(define (concatenar-cadenas lista)
  (foldr string-append "" lista))

(displayln (concatenar-cadenas '("Hola" " " "Mundo")))
; => "Hola Mundo"
(newline)

;; --------------------------------------------------
;; 9. Lista con el doble de los números > 5
;; --------------------------------------------------
(define (lista-mayores-5 lista)
  (let* ((mayores (filter (lambda (x) (> x 5)) lista))
         (dobles (map (lambda (x) (* x 2)) mayores)))
    dobles))

(displayln (lista-mayores-5 '(3 6 8 2 10)))
; => '(12 16 20)
(newline)


;; --------------------------------------------------
;; 10. Invertir una lista
;; --------------------------------------------------
(define (invertir-lista lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))

(displayln (invertir-lista '(1 2 3 4)))
; => '(4 3 2 1)
(newline)


;; --------------------------------------------------
;; 11. Aplicar función a una lista (H.O.F)
;; --------------------------------------------------
(define (aplicar-funcion f lista)
  (map f lista))

(define (cuadrado x) (* x x))

(displayln (aplicar-funcion cuadrado '(1 2 3 4)))
; => '(1 4 9 16)
(newline)


;; --------------------------------------------------
;; 12. Promedio de números mayores a 5
;; --------------------------------------------------
(define (promedio-mayores-que-5 lista)
  (let* ((mayores (filter (lambda (x) (> x 5)) lista))
         (suma (foldl + 0 mayores)))
    (/ suma (length mayores))))

(displayln (promedio-mayores-que-5 '(3 8 10 4 9 2 7)))
; => 8.5