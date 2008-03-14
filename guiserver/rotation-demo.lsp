#!/usr/bin/newlisp
;;
;; rotationn-demo.lsp - demonstrate gs:rotate-tag, gs:translate-tag and  gs:scale-tag
;; and the mouse-wheel rotating an object

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
;(gs:set-trace true)

;;;; describe the GUI
(gs:frame 'RotationDemo  100 100 600 600 "Image demo")
(gs:set-border-layout 'RotationDemo)
;(gs:set-resizable 'RotationDemo nil)
(gs:canvas 'MyCanvas)
(gs:set-background 'MyCanvas gs:white)
(gs:panel 'Select)
(gs:radio-button 'ZoomButton 'zoom-action "zoom")
(gs:radio-button 'TurnButton 'turn-action "turn")
(gs:set-selected 'TurnButton true)
(set 'turn-flag true)
(gs:add-to 'Select 'ZoomButton 'TurnButton)
(gs:add-to 'RotationDemo 'MyCanvas "center" 'Select "south")
(gs:mouse-wheel 'MyCanvas 'mouse-wheel-action)

;(gs:set-scale 0.5 0.5) ; only for testing

(gs:set-font 'MyCanvas "Lucida Sans Oblique" 14 "plain")
(gs:draw-text 'T "turn mouse wheel to turn or zoom image" 20 50 gs:darkGray)
(gs:fill-circle 'C 0 0 50 gs:red)
(gs:fill-circle 'C -40 -40 30 gs:black)
(gs:fill-circle 'C 40 -40 30 gs:black)
(gs:fill-circle 'C 0 10 8 gs:yellow)
(gs:translate-tag 'C 300 300)

(gs:set-visible 'RotationDemo true)

;; actions

(define (zoom-action id flag)
	(gs:set-selected 'TurnButton (not flag))
	(set 'turn-flag (not flag))
)

(define (turn-action id flag)
	(gs:set-selected 'ZoomButton (not flag))
	(set 'turn-flag flag)
)

(define (mouse-wheel-action x y wheel)
	(if turn-flag
		(gs:rotate-tag 'C wheel 0 0)
		(if (< wheel 0)
				(gs:scale-tag 'C 0.9 0.9)
				(gs:scale-tag 'C 1.1 1.1)
		)
	)
)


(gs:listen)

;; eof

