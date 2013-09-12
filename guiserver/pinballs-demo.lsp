#!/usr/bin/newlisp
;; pinballs-demo.lsp - demos animations and gs:check-event function

;; GS 1.05+
;; v0.3 replaced deprecated set-assoc with (setf (assoc ...) ...) 
;; v0.4 changes for 10.0 in 'inc'
;;
;; PinBalls, Cormullion wanted collision, Cormullion got collision ;-)
;;
;;
;; There are different ways to detect collisions with figures, here is an
;; example that uses a circle.
;;
;; In the current GS there is not x,y tag detection nor the option to draw
;; a circle using floats. So you have to be inventive ;-)
;;
;; This example uses gs:draw-circle and stores a imagionary boundry in BOUNDS
;; per circle/BALL. These are the collision boundry's.
;;
;; The calculation is done on the edge of the BALL calls CIRCLE-DOTS. The
;; more CIRCLE-DOTS the more detection. The more BALLS's the more calculations.
;;
;; you can change the quoted line in the code that draws its own circle!
;; if you use that you will see that GS is unable to do floats because the
;; BALL looks a little eaten. (takes more performance too!)
;;
;; Change the RADIUS to change to size of the BALL.
;;
;; BALL collision is done in 4 quadrants, this is easier then calculate them
;; on every 360 dgr. Here we take a default of 4 x 4 for speed angle.
;;
;; behaviour can change per system, if you run GS with your webbrowser open
;; (that uses JAVA or flash) you could notice stottering of the applet.
;;
;; Enjoy, Norman. (c) 2007
;; (some changes for addition to demos by L.M.)

(set-locale "C")
(seed ((now) 6))
(load (append (env "NEWLISPDIR") "/guiserver.lsp"))

(gs:init)
(gs:get-screen)
(set 'ww 600 'wh 600 'sw (gs:screen 0) 'sh (gs:screen 1))
(gs:frame 'F  (- (div sw 2) (div ww 2)) (- (div sh 2) (div wh 2)) ww (+ wh 20) "PinBalls")
(gs:canvas 'C)
(gs:set-resizable 'F nil)
(gs:set-font 'C "Monospaced" 8 "bold")
(gs:set-background 'C gs:black) 
(gs:add-to 'F 'C)
(gs:set-visible 'F true)

;; set variables
(set 'RADIUS 40 'CIRCLE-DOTS 256 'BALLS 8)
(define (rcolor) (list (random) (random) (random)))
(gs:set-stroke 3)

;; updates the current circle bounds
(define (update-circle c x y, r)
(set (sym c) '())
 (dotimes (r CIRCLE-DOTS)
  (push (list (+ x (mul RADIUS (cos r))) (+ y (mul RADIUS (sin r)))) (eval (sym c)) -1))
   (push (eval (sym c)) BOUNDS -1))

;; needs to draws my own circle
(dotimes (t BALLS)
(set 'x (+ RADIUS (rand 400)) 'y (+ RADIUS (rand 400)))
 (update-circle (string "O" t) x y)
    (gs:draw-circle (string "C" t) x y RADIUS (rcolor))
    (gs:draw-circle (string "C" t) x y ( - RADIUS 5) (rcolor))
    (gs:draw-text (string "C" t) (string t) (- x 1) y gs:white)
    (push (list (string "C" t) x y 0 3 (string "O" t)) CIRCLE -1))

;; define pseudo hit-angle action
(set 'M '(-3 -2 -1) 'P (map abs M))
(define (flip) (apply amb (append M P)))
(define (flap) (apply amb M))
(define (flop) (apply amb P))

(while (gs:check-event 10) ; timeout is small, because no events are expected
	(dolist (L CIRCLE) 
		(set 'tx (L 1))
		(set 'ty (L 2))
		(set  'X (L 3))
		(set  'Y (L 4))
		(set  'B (BOUNDS 0))

		;; check wall bounds
		(and (>= tx (- ww  RADIUS )) (set 'X (flap) 'Y (flip)) )
		(and (<= tx        RADIUS )  (set 'X (flop) 'Y (flip)) )
		(and (>= ty (- wh  RADIUS )) (set 'X (flip) 'Y (flap)) )
		(and (<= ty        RADIUS )  (set 'X (flip) 'Y (flop)) )    

		;; with every movement update the circle boundary
		(pop BOUNDS)

		(dolist (BB BOUNDS) 
	  	(and (intersect B BB)
				(or (and (<= ((BB 0) 0) tx) (<= ((BB 0) 1) ty) (set 'X  4 'Y  4 ))
					(and (>= ((BB 0) 0) tx) (<= ((BB 0) 1) ty) (set 'X -4 'Y  4 ))
					(and (<= ((BB 0) 0) tx) (>= ((BB 0) 1) ty) (set 'X  4 'Y -4 ))
					(and (>= ((BB 0) 0) tx) (>= ((BB 0) 1) ty) (set 'X -4 'Y -4 ))
				)
			)
		)
	
		(gs:move-tag (L 0) X Y 0 nil)
		(update-circle (L 5) tx ty)		

		;; update the "per" CIRCLE data	
		(setf (assoc (L 0) CIRCLE) (list (L 0) (inc tx X) (inc ty Y) X Y (L 5)))
	)
	(gs:update)
)

(exit)

