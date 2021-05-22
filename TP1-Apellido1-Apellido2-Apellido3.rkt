;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TP1-Apellido1-Apellido2-Apellido3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 1: Programas interactivos con estructuras

Integrantes:
- [Apellido, Nombre].
- [Apellido, Nombre].
- [Apellido, Nombre].
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
(define START ???)

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
(define BAR (rectangle ??? ??? "solid" "red"))

#|
Constantes asociadas a la pelota
- BALL-RADIUS: Radio de la pelota.
- BALL:        Imagen de la pelota.
|#
(define BALL-RADIUS 20)
(define BALL (circle ??? "solid" "blue"))

; Fondo
(define BACKGROUND (empty-scene WIDTH HEIGHT))

; Tamano de fuente del puntaje
(define FONT-SIZE 20)

; draw-points: st -> Image
; Toma un estado y retorna la imagen para la cantidad de puntos anotados
(define (draw-points s) (text (number->string (st-points s)) FONT-SIZE "indigo"))

; draw: COMPLETAR DISEÑO
; La función draw se encarga de dibujar el estado del sistema.
; Ubica a la pelota en la posición dada por el estado
; Ubica a la barra en su posición constante en X, y la dada por el estado en Y
; Ubica al puntaje (su límite derecho y no su centro) a 10 unidades del margen derecho.
; y a 10 unidades del margen superior (su límite superior)
; Si el puntaje y la pelota se superponen, el puntaje se debe seguir viendo
(define (draw s) ???)

; ball-next: COMPLETAR SIGNATURA
; Calcula la posición de la pelota ante un nuevo click del reloj,
; haciéndola rebotar y manteniéndola en los márgenes permitidos.
; Para ello, primero la mueve con "step" y luego corrige posibles rebotes con "bounce"
(define (ball-next s)
  (bounce (step s)))

; Caso cuando la pelota choca con la pared derecha
(check-expect (ball-next (make-st 475 200 100 6 6 1)) (make-st 479 206 100 -6 6 1))
; Caso cuando la pelota choca con la barra (+ BAR-X (/ BAR-WIDTH 2) BALL-RADIUS) = 50
(check-expect (ball-next (make-st 50 200 200 -6 6 1)) (make-st 56 206 200 6 6 2))

; step: st -> st
; Mueve la pelota según su velocidad actual, sin importar los límites de la escena
(define (step s) (make-st
   (+ (st-ball-x s) (st-ball-vx s))
   (+ (st-ball-y s) (st-ball-vy s))
   (st-bar-y s) (st-ball-vx s) (st-ball-vy s) (st-points s)))

; bounce: st -> st
; Hace rebotar la pelota en las paredes o la barra.
(define (bounce s)
  (bounce-y (bounce-x s)))

; add-point: st-> st
; Incrementa en 1 el puntaje
(define (add-point s)
  ???)

(check-expect (add-point (make-st 1 1 1 6 6 1000)) (make-st 1 1 1 6 6 1001))

; bounce-x: st -> st
; Hace rebotar la pelota en la pared derecha o la barra.
(define (bounce-x s)
  (cond
      [(hit-bar? s)        ???]
      [(hit-right-wall? s) ???]
      [else                ???]
  )
)

; hit-bar? : st -> Boolean
; Decide si la pelota colisionó con la barra
(define (hit-bar? s)
  ???)
(check-expect (hit-bar? (make-st (+ BAR-X (/ BAR-WIDTH 2) BALL-RADIUS) 200 200 -6 6 1)) #f)

; hit-right-wall? 
; Decide si la pelota colisionó con la pared derecha
(define (hit-right-wall? s)
  ???)
(check-expect (hit-right-wall? (make-st 481 200 100 6 6 1)) #t)

; bounce-y: st -> st
; Hace rebotar la pelota en la pared superior o inferior.
(define (bounce-y s)
  (cond
      [(hit-top-wall? s) ???]
      [(hit-bot-wall? s) ???]
      [else              ???]
  )
)

; hit-top-wall? 
; Decide si la pelota colisionó con la pared superior
(define (hit-top-wall? s)
  ???)

; hit-bot-wall? 
; Decide si la pelota colisionó con la pared inferior
(define (hit-bot-wall? s)
  ???)

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

; handle-key: COMPLETAR DISEÑO
(define (handle-key s k)
  (cond
    [(key=? k "down") (move-bar s BAR-VEL)]
    [(key=? k "up") (move-bar s (- BAR-VEL))]
    [else ???]))

(check-expect (handle-key (make-st 1 1 (- HEIGHT (/ BAR-VEL 2)) -1 -1 0) "down")
              (make-st 1 1 (- HEIGHT (/ BAR-HEIGHT 2)) -1 -1 0))

; move-bar: st Number -> st
; Mueve la barra n unidades, manteniéndola en los márgenes permitidos.
(define (move-bar s n)
   ???)

(check-expect (move-bar (make-st 1 1 (- HEIGHT (/ BAR-VEL 2)) -1 -1 0) BAR-VEL)
              (make-st 1 1 (- HEIGHT (/ BAR-HEIGHT 2)) -1 -1 0))


; stop?: COMPLETAR DISEÑO
(define (stop? s)
  ???)

; Imagen que aparece en caso de perder la partida
(define (ULOST s)
  (place-image (text "Juego terminado" 36 "indigo") (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

(big-bang START
  [to-draw draw]
  [on-tick ball-next]
  [on-key handle-key]
  [stop-when stop? ULOST])