;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tp1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 1: Programas interactivos con estructuras
Integrantes:
- [Apellido, Nombre].
- [Apellido, Nombre].
- [Albornoz, Martín].
|#


#|
Estructura para representar el estado del programa.
- ball-x:  La posición en x de la pelota.
- ball-y:  La posición en y de la pelota.
- bar-y:   La posición en y de la barra.
- ball-vx: La componente x de la velocidad de la pelota.
- ball-vy: La componente y de la velocidad de la pelota.
- points:  La cantidad de puntos acumulados.
|#

(define-struct st [ball-x ball-y bar-y ball-vx ball-vy points])

; Alto de la escena
(define HEIGHT 400)

; Ancho de la escena
(define WIDTH 500)

; Estado inicial
(define START (make-st 100 100 (* HEIGHT .5) 6 -6 0))

#|
Constantes asociadas a la barra
- BAR-VEL:    La velocidad de la barra.
- BAR-X:      La posición en x de la barra.
- BAR-HEIGHT: Altura de la barra.
- BAR-WIDTH:  Ancho de la barra.
- BAR:        Imagen de la barra.
|#
(define BAR-VEL 8)
(define BAR-X 20)
(define BAR-HEIGHT 50)
(define BAR-WIDTH 20)
(define BAR (rectangle BAR-WIDTH BAR-HEIGHT "solid" "red"))

#|
Constantes asociadas a la pelota
- BALL-RADIUS: Radio de la pelota.
- BALL:        Imagen de la pelota.
|#
(define BALL-RADIUS 20)
(define BALL (circle BALL-RADIUS "solid" "blue"))

; Fondo
(define BACKGROUND (empty-scene WIDTH HEIGHT))

; Tamano de fuente del puntaje
(define FONT-SIZE 20)

; Representamos los puntos acumulador como una imagen con números.
; draw-points: st -> Image
; Toma un estado y retorna la imagen para la cantidad de puntos anotados.
(define (draw-points s) (text (number->string (st-points s)) FONT-SIZE "indigo"))


; La función draw se encarga de dibujar el estado del sistema, basado en una estructura.
; draw: st -> Image
; Ubica a la pelota en la posición dada por el estado.
; Ubica a la barra en su posición constante en X, y la dada por el estado en Y.
; Ubica al puntaje (su límite derecho y no su centro) a 10 unidades del margen derecho,
; y a 10 unidades del margen superior (su límite superior).
; Si el puntaje y la pelota se superponen, el puntaje se debe seguir viendo.
(define (draw s)
  (let* (
         [points (draw-points s)]
         [points-width (image-width points)]
         [points-height (image-height points)])
    
  (place-images (list points BALL BAR)
                (list
                   (make-posn (- WIDTH 20 (/ points-width 2)) (+ 20 (/ points-height 2)))
                   (make-posn (st-ball-x s) (st-ball-y s))
                   (make-posn BAR-X (st-bar-y s)))
                   BACKGROUND)))

; ball-next toma el estado actual y devuelve un estado posterior,
; con cambios en las variables de posición.
; ball-next: st -> st
; Calcula la posición de la pelota ante un nuevo click del reloj,
; haciéndola rebotar y manteniéndola en los márgenes permitidos.
; Para ello, primero la mueve con "step" y luego corrige posibles rebotes con "bounce"
(define (ball-next s)
  (bounce (step s)))

; Caso cuando la pelota choca con la pared derecha
(check-expect (ball-next (make-st 475 200 100 6 6 1)) (make-st 479 206 100 -6 6 1))

; Caso cuando la pelota choca con la barra (+ BAR-X (/ BAR-WIDTH 2) BALL-RADIUS) = 50
(check-expect (ball-next (make-st 50 200 200 -6 6 1)) (make-st 56 206 200 6 6 2))

; step devuelve un estado posterior basado en el estado actual.
; step: st -> st
; Mueve la pelota según su velocidad actual, sin importar los límites de la escena.
(define (step s) (make-st
   (+ (st-ball-x s) (st-ball-vx s))
   (+ (st-ball-y s) (st-ball-vy s))
   (st-bar-y s) (st-ball-vx s) (st-ball-vy s) (st-points s)))

; bounce: st -> st
; Hace rebotar la pelota en las paredes o la barra.
(define (bounce s)
  (bounce-y (bounce-x s)))

; add-point suma 1 al puntaje acumulado cada vez que la pelota golpea la barra
; add-point: st-> st
; Incrementa en 1 el puntaje
(define (add-point s)
  (make-st
   (st-ball-x s)
   (st-ball-y s)
   (st-bar-y s)
   (st-ball-vx s)
   (st-ball-vy s)
   (+ (st-points s) 1)))

(check-expect (add-point (make-st 1 1 1 6 6 1000)) (make-st 1 1 1 6 6 1001))

; bounce-x: st -> st
; Hace rebotar la pelota en la pared derecha o la barra.
(define (bounce-x s)
  (let*
    ([ball-left-x (- (st-ball-x s) BALL-RADIUS)]
     [bar-right-x (+ BAR-X (/ BAR-WIDTH 2))])
    (cond
      [(hit-bar? s)        (add-point (reflect-ball-x s (- bar-right-x ball-left-x (st-ball-vx s))))]
      [(hit-right-wall? s) (reflect-ball-x s (- WIDTH (+ BALL-RADIUS 1 (st-ball-x s))))]
      [else                s]
      ))
)

; hit-bar decide si la pelota colisionó con la barra.
; hit-bar? : st -> Boolean
; Si la posición de la pelota en x menos su radio es menor que la de
; la barra y la posición en y de la pelota está entre la posición en
; y de la barra más la mitad de la altura y menos la mitad de la
; altura de la barra devuelve #true, si no se cumple alguna de estas
; dos condiciones, devuelve #false.

(define (hit-bar? s)
  (if (and
       (< (- (st-ball-x s) BALL-RADIUS) (+ BAR-X (/ BAR-WIDTH 2)))
       (> (- (st-ball-x s) BALL-RADIUS) (- BAR-X (/ BAR-WIDTH 2)))
       (> (st-ball-y s) (- (st-bar-y s) (/ BAR-HEIGHT 2)))
       (< (st-ball-y s) (+ (st-bar-y s) (/ BAR-HEIGHT 2)))
      ) #t #f))

(check-expect (hit-bar? (make-st (+ BAR-X (/ BAR-WIDTH 2) BALL-RADIUS) 200 200 -6 6 1)) #f)

; hit-right-wall decide si la pelota colisionó con la pared derecha.
; hit-right-wall?: st->Boolean 
; Decide si la pelota colisionó con la pared derecha
; Si la posición en x de la pelota más su radio es mayor o igual que
; el ancho de la escena, retorna #true, si no, retorna #false.
(define (hit-right-wall? s)
  (> (+ (st-ball-x s) BALL-RADIUS) WIDTH))

(check-expect (hit-right-wall? (make-st 481 200 100 6 6 1)) #t)

; Hace rebotar la pelota en la pared superior o inferior.
; bounce-y: st -> st
; si la pelota colisiona con la pared superior o la pared inferior,
; cambia la dirección de la velocidad en y de la pelota.
(define (bounce-y s)
  (cond
      [(hit-top-wall? s) (reflect-ball-y s BALL-RADIUS)]
      [(hit-bot-wall? s) (reflect-ball-y s (* -1 BALL-RADIUS))]
      [else              s]
  )
)

; Determina si la pelota colisionó con la pared superior.
; hit-top-wall?: st -> Boolean 
; Si la posición en y de la pelota menos su radio es menor o igual a 0,
; devuelve #true, si no, devuelve #false
(define (hit-top-wall? s)
  (if (<= (st-ball-y s) BALL-RADIUS) #t #f))

; Determina si la pelota colisionó con la pared inferior
; hit-bot-wall?: st -> Boolean 
; Si la posición en y de la pelota más su radio es mayor o igual que el
; alto de pantalla, devuelve #true, si no, devuelve #false
(define (hit-bot-wall? s)
  (if (>= (st-ball-y s) (- HEIGHT BALL-RADIUS)) #t #f))

; Cambia el sentido de la velocidad en x
; reflect-ball-x: st Number -> st
; Gira el sentido en x de la pelota y acomoda su posición en x, sumandole n unidades
; FUNCIÓN AUXILIAR - SE RECOMIENDA UTILIZARLA
(define (reflect-ball-x s n)
  (make-st
   (+ (st-ball-x s) n)
   (st-ball-y s)
   (st-bar-y s)
   (* -1 (st-ball-vx s))
   (st-ball-vy s)
   (st-points s)))

; Cambia el sentido de la velocidad en y
; reflect-ball-y: st Number -> st
; Gira el sentido en y de la pelota y acomoda su posición en y, sumandole n unidades
; FUNCIÓN AUXILIAR - SE RECOMIENDA UTILIZARLA
(define (reflect-ball-y s n)
  (make-st
   (st-ball-x s)
   (+ n (st-ball-y s))
   (st-bar-y s)
   (st-ball-vx s)
   (* -1 (st-ball-vy s))
   (st-points s)
  )
)

; Toma entradas válidas de teclado y modifica el estado
; handle-key: st String -> st
; Si la tecla preionada es "down" devuelve un st aumentando la posición en y de la barra
; Si la tecla presionada es "up" devuelve un st disminuyendo la posición en y de la barra
; En ambos casos, lo hace mediante la funcion move-bar
; En ambos casos, aumenta o disminuye un número igual a la velocidad de la barra
; Si se presiona cualquier otra tecla, devuelve el estado actual sin modificaciones
(define (handle-key s k)
  (cond
    [(key=? k "down") (move-bar s BAR-VEL)]
    [(key=? k "up") (move-bar s (- BAR-VEL))]
    [else s]))

(check-expect (handle-key (make-st 1 1 (- HEIGHT (/ BAR-VEL 2)) -1 -1 0) "down")
              (make-st 1 1 (- HEIGHT (/ BAR-HEIGHT 2)) -1 -1 0))

; Toma un st y modifica la posicion en y de la barra
; move-bar: st Number -> st
; Mueve la barra n unidades, manteniéndola en los márgenes permitidos.
; Mantiene la barra dentro de la pantalla.
(define (move-bar s n)
   (make-st
   (st-ball-x s)
   (st-ball-y s)
   (cond
     [(< (+ (st-bar-y s) n) (/ BAR-HEIGHT 2)) (/ BAR-HEIGHT 2)]
     [(> (+ (st-bar-y s) n) (- HEIGHT (/ BAR-HEIGHT 2))) (- HEIGHT (/ BAR-HEIGHT 2))]
     [else (+ (st-bar-y s) n)]
   )
   (st-ball-vx s)
   (st-ball-vy s)
   (st-points s)))

(check-expect (move-bar (make-st 1 1 (- HEIGHT (/ BAR-HEIGHT 2)) -1 -1 0) BAR-VEL)
              (make-st 1 1 (- HEIGHT (/ BAR-HEIGHT 2)) -1 -1 0))

; Determina si la pelota colisionó con la pared izquierda
; hit-left-wall?: st -> Boolean
; Si la posición en x de la pelota menos su radio es menor que 0
; devuelve #true, si no, devuelve #false
(define (hit-left-wall? s)
  (< (- (st-ball-x s) BALL-RADIUS ) 0))

; Determina si se cumplen los requisitos para ganar
; is-winner?: st -> Boolean
; Si el puntaje acumulado es mayor o igual a 10 puntos, devuelve #true, si no, devuelve #false
(define (is-winner s) (>= (st-points s) 10))

; Determina si el programa debe detenerse
; stop?: st -> Boolean
; Si hit-left-wall? o is-winner devuelven #true, stop? devolverá #true, si no, devuelve #false
(define (stop? s)
   (or (hit-left-wall? s)
       (is-winner s))
  )


; Imagen que aparece en el final de la partida.
; result: st -> Image
; Devuelve una imagen informando si se Ganó o Perdió el juego, junto con el puntaje obtenido.
(define (result s)
  (let
    ([result-message (if (is-winner s) "Ganaste," "Perdiste,")])
    (place-image (text (string-append result-message " puntaje: " (number->string (st-points s))) 36 "indigo") (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND)))

(big-bang START
  [to-draw draw]
  [on-tick ball-next]
  [on-key handle-key]
  [stop-when stop? result])