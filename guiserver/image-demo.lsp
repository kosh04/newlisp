#!/usr/bin/newlisp
;;
;; image-demo.lsp - demonstrate images rotating and zooming
;; and the mouse-wheel rotating an object

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

;;;; describe the GUI
(gs:frame 'ImageDemo  100 100 600 600 "Image demo")
(gs:set-border-layout 'ImageDemo)
;(gs:set-resizable 'ImageDemo nil)
(gs:canvas 'MyCanvas)
(gs:set-background 'MyCanvas gs:white)
(gs:panel 'Select)
(gs:radio-button 'ZoomButton 'zoom-action "zoom")
(gs:radio-button 'TurnButton 'turn-action "turn")
(gs:set-selected 'TurnButton true)
(set 'turn-flag true)
(gs:add-to 'Select 'ZoomButton 'TurnButton)
(gs:add-to 'ImageDemo 'MyCanvas "center" 'Select "south")
(gs:mouse-wheel 'MyCanvas 'mouse-wheel-action)

;(gs:set-scale 0.5 0.5) ; only for testing

(gs:set-font 'MyCanvas "Lucida Sans Oblique" 14 "plain")
(gs:draw-text 'T "turn mouse wheel to turn or zoom image" 20 50 gs:darkGray)
(gs:draw-image 'S "/local/newLISP128.png" 320 20 32 50)
(gs:draw-image 'S "/local/newLISP128.png" 380 30 50 32)

(gs:draw-image 'I "/local/newLISP128.png" -64 -64)
(gs:translate-tag 'I 300 300)

(gs:set-visible 'ImageDemo true)

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
		(gs:rotate-tag 'I wheel 0 0)
		(if (< wheel 0)
			(gs:scale-tag 'I 0.9 0.9)
			(gs:scale-tag 'I 1.1 1.1)
		)
	)
)


(gs:listen)

;; eof

