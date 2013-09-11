#!/usr/bin/newlisp
;;
;; animation-demo.lsp - demonstrate gs:move-tag for making animations
;; and the mouse-wheel moving a text object

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

;;;; describe the GUI
(gs:frame 'AnimationDemo 100 100 400 400 "Animation ~ 50 frames/sec, 5  pixel/step")
(gs:set-resizable 'AnimationDemo nil)
(gs:canvas 'MyCanvas)
(gs:set-background 'MyCanvas gs:white)
(gs:mouse-wheel 'MyCanvas 'mouse-wheel-action)
(gs:add-to 'AnimationDemo 'MyCanvas)

(set 'Ty 200)
(gs:set-font 'MyCanvas "Lucida Sans Oblique" 16 "plain")
(gs:draw-text 'T "mouse wheel moves this text, changes speed" 20 Ty)
(gs:fill-circle 'C 50 40 25 gs:red)
(gs:fill-circle 'C 30 20 15 gs:black)
(gs:fill-circle 'C 70 20 15 gs:black)
(gs:fill-circle 'C 50 48 6 gs:yellow)

(gs:set-visible 'AnimationDemo true)


(define (mouse-wheel-action x y wheel)
		(inc delay (* wheel 1000))
		(if (> delay 80000) (set 'delay 80000))
		(if (< delay 500) (set 'delay 500))
        (if (= ostype "OSX")
                (gs:move-tag 'T 0 (* wheel wheel (sgn wheel)))
                (gs:move-tag 'T 0 (* 5 wheel)))
)

(set 'delay 20000) ;;

(while (gs:check-event delay)
	(dotimes (x 60)
		(gs:check-event delay)
		(gs:move-tag 'C 5 0))
	(dotimes (x 60)
		(gs:check-event delay)
		(gs:move-tag 'C 0 5))
	(dotimes (x 60)
		(gs:check-event delay)
		(gs:move-tag 'C -5 0))
	(dotimes (x 60)
		(gs:check-event delay)
		(gs:move-tag 'C 0 -5))
)

(exit)

;; eof

