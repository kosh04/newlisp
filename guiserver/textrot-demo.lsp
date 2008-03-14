#!/usr/bin/newlisp
;;
;; textrot-demo.lsp - demonstrate gs:rotate-tag on text
;; and the mouse-wheel rotating an object

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
;(gs:set-trace true)

;;;; describe the GUI
(gs:frame 'RotationDemo 100 100 400 300 "Rotate text and widgets on a canvas")
(gs:set-resizable 'RotationDemo nil)
(gs:canvas 'MyCanvas 'RotationDemo)
(gs:button 'TheButton 'button-action "Exit")
(gs:set-background 'TheButton gs:white)
(gs:add-to 'MyCanvas 'TheButton)
(gs:set-stroke 3.0)
(gs:set-background 'MyCanvas gs:white)
(gs:mouse-wheel 'MyCanvas 'mouse-wheel-action)
(gs:add-to 'RotationDemo 'MyCanvas)

(gs:set-font 'MyCanvas "Lucida Sans Oblique" 36 "plain")
(gs:fill-rect 'R 200 100 60 60 gs:lightGray 0.1)
(gs:draw-text 'T "turn mouse wheel" 50 130 gs:red -15)
(gs:fill-rect 'R 100 100 60 60 gs:lightGray 0.1)

(gs:set-visible 'RotationDemo true)

(define (mouse-wheel-action x y wheel)
	(gs:rotate-tag 'T wheel 50 130)
)

(define (button-action)
	(exit 0))

(gs:listen)

;; eof

