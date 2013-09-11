#!/usr/bin/newlisp
;;
;; shapes-demo.lsp - demonstrate different lines, outlines and shapes
;; v.1.0

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

;; subroutines for random shapes

(define (random-draw-line)
	(gs:draw-line 'L (rand 640) (rand 640) (rand 640) (rand 640) (list (random) (random) (random))))

(define (random-draw-rect)
	(gs:draw-rect 'R (rand 640) (rand 640) (rand 100) (rand 100) (list (random) (random) (random))))

(define (random-fill-rect)
	(gs:fill-rect 'R (rand 640) (rand 640) (rand 100) (rand 100) (list (random) (random) (random))))

(define (random-draw-round-rect)
	(gs:draw-round-rect 'R (rand 640) (rand 640) (rand 100) (rand 100) 
			(rand 40) (rand 40) (list (random) (random) (random))))

(define (random-fill-round-rect)
	(gs:fill-round-rect 'R (rand 640) (rand 640) (rand 100) (rand 100) 
			(rand 40) (rand 40) (list (random) (random) (random))))

(define (random-draw-circle)
	(gs:draw-circle 'C (rand 640) (rand 640) (rand 100)  (list (random) (random) (random))))

(define (random-fill-circle)
	(gs:fill-circle 'C (rand 640) (rand 640) (rand 100)  (list (random) (random) (random))))

(define (random-draw-ellipse)
	(gs:draw-ellipse 'E (rand 640) (rand 640) (rand 100)  (rand 100) (list (random) (random) (random))))

(define (random-fill-ellipse)
	(gs:fill-ellipse 'E (rand 640) (rand 640) (rand 100)  (rand 100) (list (random) (random) (random))))

(define (random-draw-arc)
	(gs:draw-arc 'A (rand 640) (rand 640) (rand 100) (rand 100) (rand 360) (rand 360) (list (random) (random) (random))))

(define (random-fill-arc)
	(gs:fill-arc 'A (rand 640) (rand 640) (rand 100) (rand 100) (rand 360) (rand 360) (list (random) (random) (random))))

(gs:init) 
;(gs:set-trace true)

;;;; describe the GUI
(gs:frame 'ShapesDemo 100 100 640 640 "Random lines, rectangles, circles, ellipses and arcs Demo")
(gs:set-border-layout 'ShapesDemo)
(gs:canvas 'MyCanvas 'ShapesDemo)
(gs:panel 'Selection)
(gs:label 'HelpText "show or hide shapes:")
(gs:check-box 'LineSelect 'select-action "lines") 
(gs:check-box 'RectangleSelect 'select-action "rectangles") 
(gs:check-box 'CircleSelect 'select-action "circles") 
(gs:check-box 'EllipseSelect 'select-action "ellipse") 
(gs:check-box 'ArcSelect 'select-action "arcs") 
(gs:set-selected 'LineSelect true 'RectangleSelect true 'CircleSelect true 'EllipseSelect true 'ArcSelect true)
(gs:add-to 'Selection 'HelpText 'LineSelect 'RectangleSelect 'CircleSelect 'EllipseSelect 'ArcSelect)
(gs:add-to 'ShapesDemo 'MyCanvas "center" 'Selection "south")
(gs:set-background 'MyCanvas gs:white)

; default color if not specified in shape or text
(gs:set-paint gs:darkGray) 

;(gs:set-translation 100 100) ;only for test, will shift everything
;(gs:set-scale 0.5 0.5) ; only for testing scrinks or zooms
;(gs:set-rotation 10) ; only for testing tilts by 10 degree

(set 'N 20)

(set 'N-shapes (* N 11))

(set 'start (time-of-day))
(dotimes (i N)
	(random-draw-line)
	(random-draw-rect)
	(random-fill-rect)
	(random-draw-round-rect)
	(random-fill-round-rect)
	(random-draw-circle)
	(random-fill-circle)
	(random-draw-ellipse)
	(random-fill-ellipse)
	(random-draw-arc)
	(random-fill-arc)
)

(println N-shapes " shapes " (div (- (time-of-day) start) N-shapes) "ms per shape")

(gs:set-font 'MyCanvas "Lucida Sans Regular" 40 "italic")
(gs:draw-text 'T "Random"  60 100)
(gs:set-font 'MyCanvas "Monospaced" 40 "plain")
(gs:draw-text 'T "Shapes and Outlines"  60 160 gs:green -15)
;(gs:draw-text 'T "Third text line"  60 220) ; only for testing

(gs:set-visible 'ShapesDemo true)

;; action handler

(define (select-action id flag)
	(let (tag (case id
			("MAIN:LineSelect" 'L)
			("MAIN:RectangleSelect" 'R)
			("MAIN:CircleSelect" 'C)
			("MAIN:EllipseSelect" 'E)
			("MAIN:ArcSelect" 'A)))
		(if flag 
			(gs:show-tag tag) 
			(gs:hide-tag tag))
	)
)

(gs:add-to 'Selection 'HelpText 'LineSelect 'RectangleSelect 'CircleSelect 'EllipseSelect 'ArcSelect)
;;;; listen for incoming action requests and dispatch

;(gs:export "shapes.png") ; just for testing

(gs:listen)

;; eof

