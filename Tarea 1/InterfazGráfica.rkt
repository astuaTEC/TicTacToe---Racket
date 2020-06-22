#lang racket
(require graphics/graphics)
(open-graphics)


;;----------------------------Elementos de inicialización----------------------------------------------------
  
;; Anchura de la ventana de gráficos
(define horizontal 900)

;; Altura de la ventana de gráficos
(define vertical 700)

;; Se crea una ventana gráfica
(define ventana (open-viewport "ventana" horizontal vertical) )

;;Se crea una ventana oculta pixmap
(define oculta (open-pixmap "oculta" horizontal vertical))

;Se crea un rectángulo negro de fondo
((draw-solid-rectangle oculta) (make-posn 0 0) horizontal vertical "black")

;Se muestra un texto con las instrucciones para inicializar el juego
((draw-string oculta) (make-posn 200 300) "Inicializa el juego poniendo en la consola (TTT m n), donde m y n son números" "white")
((draw-string oculta) (make-posn 200 330) "Deben ser números entre 3 y 10" "white")


;---------------------------------------------------------------------------------------------------------------------------------------------

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
  (cond ((equal? m 0)
         #t)
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
;; en la ventana del juego, obteniendo las coordenadas X y Y en pixeles
;; m: tamaño de filas disponible para dibujar (numero)
;; n: tamaño de columnas disponible para dibujar (numero)
;; lista: lista de coordenadas ocupadas por alguna figura, inicialmente vacia. Ej: ( (2 1) (1 1) )

(define (mouse m n lista)
  (let* ((click (get-mouse-click ventana))
         (x (posn-x (query-mouse-posn ventana)))
         (y (posn-y (query-mouse-posn ventana))))
  (cond ((equal? (left-mouse-click? click)
         (mouse m n (pegar lista (list (verificarDibujoX x y m n lista))))))
  (else
   (mouse m n lista)))))


;; Función para dibujar el síbolo X en pantalla
;; x: posición en x donde se quiere poner la imagen
;; y: posición en y donde se quiere poner la imagen

(define (dibujarX x y)
  ((draw-pixmap oculta) "x.png" (make-posn (+ 35 (* 65 x)) (- (* 65 y) 55)))
  (copy-viewport oculta ventana))


;; Función para dibujar el síbolo O en pantalla
;; x: posición en x donde se quiere poner la imagen
;; y: posición en y donde se quiere poner la imagen

(define (dibujarO x y)
  ((draw-pixmap oculta) "circulo.png" (make-posn (+ 35 (* 65 x)) (- (* 65 y) 55)))
  (copy-viewport oculta ventana))


;; Función que devuelve la fila
;; correspondiente de acuerdo a un rango
;; de pixeles en y
;; y: numero entero (posición en y dada en pixeles)

(define (darFila y)
  (cond ((and (<= 10 y) (> 75 y))
         '(1))
        ((and (<= 75 y) (> 140 y))
         '(2))
        ((and (<= 140 y) (> 205 y))
         '(3))
        ((and (<= 205 y) (> 270 y))
         '(4))
        ((and (<= 270 y) (> 335 y))
         '(5))
        ((and (<= 335 y) (> 400 y))
         '(6))
        ((and (<= 400 y) (> 465 y))
         '(7))
        ((and (<= 465 y) (> 530 y))
         '(8))
        ((and (<= 530 y) (> 595 y))
         '(9))
        ((and (<= 595 y) (> 660 y))
         '(10))
        (else
         #f)))


;; Función que devuelve la columna
;; correspondiente de acuerdo a un rango
;; de pixeles en x
;; x: numero entero (posición en x dada en pixeles)

(define (darCol x)
  (cond ((and (<= 100 x) (> 165 x))
         '(1))
        ((and (<= 165 x) (> 230 x))
         '(2))
        ((and (<= 230 x) (> 295 x))
         '(3))
        ((and (<= 295 x) (> 360 x))
         '(4))
        ((and (<= 360 x) (> 425 x))
         '(5))
        ((and (<= 425 x) (> 490 x))
         '(6))
        ((and (<= 490 x) (> 555 x))
         '(7))
        ((and (<= 555 x) (> 620 x))
         '(8))
        ((and (<= 620 x) (> 685 x))
         '(9))
        ((and (<= 685 x) (> 750 x))
         '(10))
        (else
         #f)))


;; Función para verificar si la
;; cuadrícula disponible permite
;; poner una X en la posición que el usuario hizo click
;; x: coordenada X donde se quiere poner la figura (en pixeles)
;; y: coordenada Y donde se quiere poner la figura (en pixeles)
;; m: tamaño de filas disponible para dibujar (numero)
;; n: tamaño de columnas disponible para dibujar (numero)
;; lista: lista de coordenadas ocupadas por alguna figura. Ej: ( (2 1) (1 1) )
;; Retorna el punto donde se dibuja la figur. Ej: (3 2) donde el 3 es la fila y el 2 la columna 

(define (verificarDibujoX x y m n lista)
  (let* ((fila (darFila y))
         (col (darCol x))
         (punto (cons (car fila) col )))
  (cond (( and (<= (car fila) m) (<= (car col) n) (not (miembro? punto lista)))
         (dibujarX (car col) (car fila))
         punto)
        (else
         (msj "Clickee en otra parte")))))
         


;; Función para mostrar ventanas de alerta
;; txt: Es un string, siendo este el texto a mostrar

(define (msj texto)
  (define ventana2 (open-viewport "Alerta" 300 50))
  ((draw-viewport ventana2) "black")
  ((draw-string ventana2) (make-posn 50 20) texto "red")
  (sleep 2)
  (close-viewport ventana2))


;; Funcion para encontar si un elemento se encuentra
;; en una lista
;; ele: símbolo cualquiera
;; lista: una lista lineal

(define (miembro? ele lista)
  (cond( (null? lista)
         #f)
       ( (equal? ele (car lista))
         #t)
       (else
        (miembro? ele (cdr lista)))))

;; Funcion para pegar dos listas en el orden
;; lista1, lista2: lista lineales

(define (pegar lista1 lista2)
  (cond((null? lista1)
        lista2)
       ((null? lista2)
        lista1)
       (else
        (cons (car lista1)
              (pegar (cdr lista1) lista2)))))


;; Función que inicializa el juego
;; Lo que hace es llamar las funciones que crean
;; el tablero TicTacToe gráfico

(define (TTT m n)
  (cond ((and (>= m 3) (<= m 10) (>= n 3) (<= n 10))
         ((draw-solid-rectangle oculta) (make-posn 0 0) horizontal vertical "black")
         (crearLineas m n)(copy-viewport oculta ventana)
         (mouse m n '()))
        (else
         (msj "Inserte números válidos"))))



(copy-viewport oculta ventana)





