#lang racket
(require graphics/graphics)
(require racket/draw)
(open-graphics)

(require racket/include)
(require "TicTacToeFull.rkt")


;;-------------------------------------------------------------------------------------------------------------------------------
;; Código Fuente: InterfazGráfica.rkt
;; Desarrolado por: Saymon Astúa, Oscar Araya
;; Tarea 1 - Lenguajes
;; IS 2020 - Grupo 01
;;
;; Descripción: Interfaz gráfica del juego TicTacToe
;; Instituto Tecnológico de Costa Rica | CE3104 - Lenguajes, Compiladores e Interpretes

;;------------------------------------------------ELEMENTOS DE INICIALIZACION----------------------------------------------------
  
;; Anchura de la ventana de gráficos
(define horizontal 1100)

;; Altura de la ventana de gráficos
(define vertical 700)

;; Se crea una ventana gráfica
(define ventana (open-viewport "TicTacToe" horizontal vertical) )

;;Se crea una ventana oculta pixmap
(define oculta (open-pixmap "oculta" horizontal vertical))

;Se crea un rectángulo negro de fondo
((draw-solid-rectangle oculta) (make-posn 0 0) horizontal vertical "black")

;; Se agregan imágenes de inicializacion del juego
((draw-pixmap oculta) "imgs/bv.jpg" (make-posn 200 10))
(copy-viewport oculta ventana)
(sleep 2)

((draw-pixmap oculta) "imgs/TTT-1.png" (make-posn 600 100))
(copy-viewport oculta ventana)
(sleep 2)

;Se muestra un texto con las instrucciones para inicializar el juego
((draw-pixmap oculta) "imgs/inst.png" (make-posn 100 200))

;;Esta es la matriz logica que se trabaja en el juego
(define matriz '() )

;-------------------------------------------------------TABLERO GRÁFICO---------------------------------------------------------

;; Funcion para crear el tablero TicTacToe
;; m: número de filas. Número entero
;; n: número de columnas. Numéro entero

(define (crearLineas m n)
  (cond ((and (>= m 3) (<= m 10) (>= n 3) (<= n 10))
      (dibujarFilas (- n 1) m)
      (dibujarCol (- m 1) n))

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


;-------------------------------------------DIBUJAR FICHAS EN LA PANTALLA----------------------------------------------------------

;; Función para dibujar el síbolo X en pantalla
;; x: posición en x donde se quiere poner la imagen
;; y: posición en y donde se quiere poner la imagen

(define (dibujarX x y)
  ((draw-pixmap oculta) "imgs/x-1.png" (make-posn (+ 45 (* 65 x)) (- (* 65 y) 45)))
  (copy-viewport oculta ventana))


;; Función para dibujar el síbolo O en pantalla
;; x: posición en x donde se quiere poner la imagen
;; y: posición en y donde se quiere poner la imagen

(define (dibujarO x y)
  ((draw-pixmap oculta) "imgs/circulo-1.png" (make-posn (+ 45 (* 65 x)) (- (* 65 y) 45)))
  (copy-viewport oculta ventana))


;----------------------------------------------FUNCIONES PARA IMPLEMENTAR EL MOUSE------------------------------------------------------

;; Función para saber si se hace click izquierdo
;; Es prácticamente el ciclo principal del juego, desde donde se hacen llamadas a las demás funciones
;; en la ventana del juego, obteniendo las coordenadas X y Y en pixeles
;; m: tamaño de filas disponible para dibujar (numero)
;; n: tamaño de columnas disponible para dibujar (numero)
;; lista: lista de coordenadas ocupadas por alguna figura, inicialmente vacia. Ej: ( (2 1) (1 1) )

(define (mouse m n lista)
  (let* ((click (get-mouse-click ventana))
         (x (posn-x (query-mouse-posn ventana)))
         (y (posn-y (query-mouse-posn ventana))))
  (cond ((equal? (left-mouse-click? click)
         (mouse m n (append lista (verificarDibujoX x y m n lista))  )))
  (else
   (mouse m n lista )))))


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
         (list y))))


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
         (list x))))

;---------------------------------------------VERIFICACION DE LOS CLICKS Y EJECUCIÓN DE TURNOS-------------------------------------------------------------------

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
         (punto (cons (car col) fila)))
  (cond (( and (<= (car fila) n) (<= (car col) m) (not (miembro? punto lista)))
         (dibujarX (car col) (car fila))
         (turnoJugador col fila n)
         (let* ((resultado turnoMaquina))
           (cond ((null? resultado)
                  (list punto))
                 (else
                  (append (list (turnoMaquina)) (list punto))))))
        (else
         (alerta)
         '()))))
         

;; Función para poner una ficha por parte del jugador
;; lo que hace es poner dentro de una matriz lógica la ficha
;; que el jugador quiere poner en pantalla
;; Se valida si existe un empate o una victoria
;; solamente actualiza la matriz logica, no retorna nada

(define (turnoJugador col fila m)
  (let* ( (resultado (colocarFichaJugador (car fila) (car col) 1 m matriz matriz '() 1))
          (linea (car resultado))
          (pos (cadr resultado))
          (nuevaMatriz (caddr resultado)))
    (cond ((null? linea)
           (verificarEmpate)
           (set! matriz nuevaMatriz))
          (else
           (dibujarLineaHD (cadar linea) (caar linea) (cadadr linea)(caadr linea))
           (dibujarLineaV  (cadar linea) (caar linea)  (cadadr linea) (caadr linea))
           (ventanaGanador)))))
           
           
           
;;Función para colocar la ficha de la máquina
;; Esta tiene que llamar al algoritmo voraz
;; En general lo que hace es colocar la ficha de la máquina en la interfaz
;; de acuerdo al algoritmo voraz
;; Antes de eso, se verifica si existe un empate o si la máquina ha ganado
;; La función retorna una posición de la matriz donde es colocada la ficha. Ej: (2 4), fila 2, columna 4

(define (turnoMaquina)
  (cond ((equal? (verificarEmpate) #f)
         (let* ( (resultado (colocarFichaMaquina matriz 2))
          (linea (car resultado))
          (posicion (cadr resultado))
          (matrizNueva (caddr resultado)))
           
          (cond ((null? linea)
                 (dibujarO (cadr posicion) (car posicion))
                 (set! matriz matrizNueva)
                 (verificarEmpate)
                 (list (cadr posicion) (car posicion)))
                (else
                 (dibujarO (cadr posicion) (car posicion))
                 (dibujarLineaHD (cadar linea) (caar linea) (cadadr linea)(caadr linea))
                 (dibujarLineaV  (cadar linea) (caar linea)  (cadadr linea) (caadr linea))
                 (ventanaPerdedor)
                 '()))))
        )
  )
        

;; funcion verificar empate
;; Si la el tablero está lleno, devuelve #t y se muestra un empate en pantalla
;; En caso contrario, devuelve #f

(define (verificarEmpate)
  (cond (( equal? (matrizLlena matriz) #t)
         (ventanaEmpate)
         #t)
        (else
         #f)))


;---------------------------------------------------FUNCIONES PARA CREAR VENTANAS------------------------------------------------------------


;; Función para mostrar ventanas de alerta
;; txt: Es un string, siendo este el texto a mostrar

(define (msj texto)
  (define ventana1 (open-viewport "Alerta" 300 50))
  ((draw-viewport ventana1) "black")
  ((draw-string ventana1) (make-posn 50 20) texto "red")
  (sleep 2)
  (close-viewport ventana1))

;; Función para mostrar ventanas de alerta
;; Click fuera del rango

(define (alerta)
  (define ventana2 (open-viewport "Alerta" 300 150))
  ((draw-viewport ventana2) "black")
  ((draw-pixmap ventana2) "imgs/alerta.png" (make-posn 0 0))
  (sleep 1.5)
  (close-viewport ventana2))


;; Función para mostrar la ventana del ganador

(define (ventanaGanador)
  (define ventana3 (open-viewport "Ganador" 600 320))
  ((draw-viewport ventana3) "black")
  ((draw-pixmap ventana3) "imgs/ganador.png" (make-posn 0 0))
  (sleep 4)
  (close-viewport ventana)
  (close-viewport ventana3))


;; Función para mostrar la ventana del perdedor

(define (ventanaPerdedor)
  (define ventana4 (open-viewport "Perdedor" 600 320))
  ((draw-viewport ventana4) "black")
  ((draw-pixmap ventana4) "imgs/perdedor.png" (make-posn 0 0))
  (sleep 4)
  (close-viewport ventana)
  (close-viewport ventana4))


;; Función para mostrar la ventana en caso de empate

(define (ventanaEmpate)
  (define ventana5 (open-viewport "Empate" 600 320))
  ((draw-viewport ventana5) "black")
  ((draw-pixmap ventana5) "imgs/empate.png" (make-posn 0 0))
  (sleep 4)
  (close-viewport ventana)
  (close-viewport ventana5))

;----------------------------------------------------LINEAS GRAFICAS--------------------------------------------------------------------

;;Funcion para dibijar una linea entre 2 puntos (Horizontal y Diagonal)
;; Recibe un col1 , fila1 ,col2, fila2
;; col1, fila1: posiciones desde donde sale la linea (posiciones en la matriz)
;; col2, fila2: posiciones hasta donde llega la linea (posiciones en la matriz)

(define (dibujarLineaHD col1 fila1 col2 fila2)
  ((draw-line oculta) (make-posn (+ 65 (* 65 col1)) (- (* 65 fila1) 27)) (make-posn (+ 65 (* 65 col2)) (- (* 65 fila2) 27)) "black")
  ((draw-line oculta) (make-posn (+ 65 (* 65 col1)) (- (* 65 fila1) 26)) (make-posn (+ 65 (* 65 col2)) (- (* 65 fila2) 26)) "black")
  ((draw-line oculta) (make-posn (+ 65 (* 65 col1)) (- (* 65 fila1) 25)) (make-posn (+ 65 (* 65 col2)) (- (* 65 fila2) 25)) "black")
  ((draw-line oculta) (make-posn (+ 65 (* 65 col1)) (- (* 65 fila1) 24)) (make-posn (+ 65 (* 65 col2)) (- (* 65 fila2) 24)) "black")
  ((draw-line oculta) (make-posn (+ 65 (* 65 col1)) (- (* 65 fila1) 23)) (make-posn (+ 65 (* 65 col2)) (- (* 65 fila2) 23)) "black")
  (copy-viewport oculta ventana))
 

;;Funcion para dibijar una linea entre 2 puntos (Vertical)
;; Recibe un col1 , fila1 ,col2, fila2
;; col1, fila1: posiciones desde donde sale la linea (posiciones en la matriz)
;; col2, fila2: posiciones hasta donde llega la linea (posiciones en la matriz)

(define (dibujarLineaV col1 fila1 col2 fila2)
  ((draw-line oculta) (make-posn (+ 65 (* 65 col1)) (- (* 65 fila1) 25)) (make-posn (+ 65 (* 65 col2)) (- (* 65 fila2) 25)) "black")
  ((draw-line oculta) (make-posn (+ 66 (* 65 col1)) (- (* 65 fila1) 25)) (make-posn (+ 66 (* 65 col2)) (- (* 65 fila2) 25)) "black")
  ((draw-line oculta) (make-posn (+ 67 (* 65 col1)) (- (* 65 fila1) 25)) (make-posn (+ 67 (* 65 col2)) (- (* 65 fila2) 25)) "black")
  ((draw-line oculta) (make-posn (+ 68 (* 65 col1)) (- (* 65 fila1) 25)) (make-posn (+ 68 (* 65 col2)) (- (* 65 fila2) 25)) "black")
  ((draw-line oculta) (make-posn (+ 69 (* 65 col1)) (- (* 65 fila1) 25)) (make-posn (+ 69 (* 65 col2)) (- (* 65 fila2) 25)) "black")
  (copy-viewport oculta ventana))
 

;------------------------------------------FUNCIONES AUXILIARES DE IMPLEMENTACION------------------------------------------------------------

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

;-------------------------------------------------FUNCION DE INICIALIZACION DEL JUEGO-----------------------------------------------------

;; Función que inicializa el juego
;; Lo que hace es llamar las funciones que crean
;; el tablero TicTacToe gráfico

(define (TTT m n)
  (cond ((and (>= m 3) (<= m 10) (>= n 3) (<= n 10))
         ;; Se inicializan ciertos elementos gráficos
         
         ((draw-solid-rectangle oculta) (make-posn 0 0) horizontal vertical "white")
         ((draw-pixmap oculta) "imgs/fondo.jpg" (make-posn 800 0))
         ((draw-solid-rectangle oculta) (make-posn 850 65) 180 30 "black")
         ((draw-string oculta) (make-posn 900 85) "TU FICHA ES:" "red")
         ((draw-pixmap oculta) "imgs/x-22.png" (make-posn 880 100))
         ((draw-solid-rectangle oculta) (make-posn 800 350) 300 8 "white")
         ((draw-solid-rectangle oculta) (make-posn 855 380) 180 30 "black")
         ((draw-string oculta) (make-posn 875 400) "LA FICHA RIVAL ES:" "red")
         ((draw-pixmap oculta) "imgs/circulo-22.png" (make-posn 880 415))

         ;;se crea el tablero gráfico
         (crearLineas m n)(copy-viewport oculta ventana)


         ;Se llama a la parte lógica (TTTB m n)
         ;devuelve la matriz a trabajar
         (set! matriz (TTTB m n))

         (mouse m n '()))
        
        (else
         (msj "Inserte números válidos"))))




(copy-viewport oculta ventana)


