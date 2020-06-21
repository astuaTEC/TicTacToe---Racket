#lang racket
(require graphics/graphics)

(open-graphics)


  
;; Anchura de la ventana de gráficos
(define horizontal 1200)
;; Altura de la ventana de gráficos
(define vertical 700)
;; Se crea una ventana gráfica
(define ventana (open-viewport "ventana" horizontal vertical) )
;;Se crea una ventana oculta pixmap
(define oculta (open-pixmap "ejemplo" horizontal vertical))
;Se crea un rectángulo negro de fondo
((draw-solid-rectangle oculta) (make-posn 0 0) horizontal vertical "black")

;; Funcion para crear el tablero TicTacToe
;; m: número de filas. Número entero
;; n: número de columnas. Numéro entero

(define (crearLineas m n)
  (cond ((and (>= m 3) (<= m 10) (>= n 3) (<= n 10))
      (dibujarFilas (- m 1) n)
      (dibujarCol (- n 1) m))

        (else
         #f)))

;; Función para crear las lineas que mustran
;; la división de filas en el tablero
;; m: número de filas. Número entero
;; largo: largo de la línea. Número entero

(define (dibujarFilas m largo)
  (cond ((equal? m 0) #t)

        (else
         ((draw-solid-rectangle oculta) (make-posn 100 (+ (* m 65) 10)) (* largo 65) 4  "green")
         (dibujarFilas (- m 1) largo))))
   

;; Función para crear las lineas que mustran
;; la división de columnas en el tablero
;; n: número de columnas. Número entero
;; largo: largo de la línea. Número entero

(define (dibujarCol n largo)
  (cond ((equal? n 0) #t)
        (else
         ((draw-solid-rectangle oculta) (make-posn (+ (* n 65) 100) 10) 4 (* largo 65) "green")
         (dibujarCol (- n 1) largo))))



;; Función para saber si se hace click izquierdo
;; en la ventana del juego

(define (mouse)
  (let* ((click (get-mouse-click ventana))
         (x (posn-x (query-mouse-posn ventana)))
         (y (posn-y (query-mouse-posn ventana))))
  (cond ((equal? (left-mouse-click? click)
         (print x)
         (display y)
         (dibujarX x y)
         (mouse)))
  (else
   (mouse)))))


;; Función para dibujar el síbolo X en pantalla
;; x: posición en x donde se quiere poner la imagen
;; y: posición en y donde se quiere poner la imagen

(define (dibujarX x y)
  ((draw-pixmap oculta) "x.png" (make-posn (- x 32) (- y 32)))
  (copy-viewport oculta ventana))

;; Función para dibujar el síbolo O en pantalla
;; x: posición en x donde se quiere poner la imagen
;; y: posición en y donde se quiere poner la imagen

(define (dibujarO x y)
  ((draw-pixmap oculta) "C:/Users/DELL/Documents/RacketProjects/Tarea 1/circulo.png" (make-posn x y))
  (copy-viewport oculta ventana))



(crearLineas 10 10)

(copy-viewport oculta ventana)

(mouse)




