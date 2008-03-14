#!/usr/bin/newlisp
;;
;; animation-demo.lsp - demonstrate gs:move-tag for making animations
;; and the mouse-wheel moving a text object

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

;;;; describe the GUI
(gs:frame 'AnimationDemo 100 100 400 300 "Animation ~ 30 frames/sec, 5  pixel/step")
(gs:set-resizable 'AnimationDemo nil)
(gs:canvas 'MyCanvas)
(gs:set-background 'MyCanvas gs:white)
(gs:mouse-wheel 'MyCanvas 'mouse-wheel-action)
(gs:add-to 'AnimationDemo 'MyCanvas)

(set 'Ty 140)
(gs:set-font 'MyCanvas "Lucida Sans Oblique" 16 "plain")
(gs:draw-text 'T "turn the mouse wheel to move this text" 20 Ty)
(gs:fill-circle 'C 50 40 25 gs:red)
(gs:fill-circle 'C 30 20 15 gs:black)
(gs:fill-circle 'C 70 20 15 gs:black)
(gs:fill-circle 'C 50 48 6 gs:yellow)

(gs:set-visible 'AnimationDemo true)


(define (mouse-wheel-action x y wheel)
        (if (= ostype "OSX")
                (gs:move-tag 'T 0 (* wheel wheel (sgn wheel)))
                (gs:move-tag 'T 0 (* 5 wheel)))
)

(set 'delay 33000) ;;

(while (gs:check-event delay)
	(dotimes (x 60)
		(gs:check-event delay)
		(gs:move-tag 'C 5 0))
	(dotimes (x 40)
		(gs:check-event delay)
		(gs:move-tag 'C 0 5))
	(dotimes (x 60)
		(gs:check-event delay)
		(gs:move-tag 'C -5 0))
	(dotimes (x 40)
		(gs:check-event delay)
		(gs:move-tag 'C 0 -5))
)

(exit)

;; eof

