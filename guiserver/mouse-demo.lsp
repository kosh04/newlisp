#!/usr/bin/newlisp
;;
;; mouse-demo.lsp - demonstrate mouse handlers and gs:delete-tag

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

(set 'colors (list gs:black gs:blue gs:cyan gs:darkGray gs:gray 
				gs:green gs:lightGray gs:magenta gs:orange gs:pink gs:red gs:yellow))

;;;; describe the GUI
(gs:frame 'MouseDemo 100 100 640 640 "Canvas Demo")
(gs:canvas 'MyCanvas 'MouseDemo)
(gs:add-to 'MouseDemo 'MyCanvas)
(gs:set-background 'MyCanvas gs:white)
(gs:mouse-pressed 'MyCanvas 'mouse-pressed-action true)
(gs:mouse-released 'MyCanvas 'mouse-released-action true)
(gs:mouse-clicked 'MyCanvas 'mouse-clicked-action true)
(gs:mouse-moved 'MyCanvas 'mouse-moved-action)
(gs:mouse-dragged 'MyCanvas 'mouse-dragged-action)
(gs:mouse-wheel 'MyCanvas 'mouse-wheel-action)
(gs:set-anti-aliasing true)
(println (time-of-day))
(for (row 0 620 20)
	(for (col 0 620 20)
		(let (r (rand (length colors)))
			(gs:fill-rect (string "R" r) col row 20 20 (colors r))
		)
	)
)
(println (time-of-day))
(gs:set-visible 'MouseDemo true)

;; define actions

(define (mouse-pressed-action x y button modifiers tags)
	(gs:set-text 'MouseDemo 
		(string "pressed row: " (/ y 20) " col:" (/ x 20) " button: " button " key:" modifiers " tags:" tags))
)

(define (mouse-released-action x y button modifiers tags)
	(gs:set-text 'MouseDemo 
		(string "released row: " (/ y 20) " col:" (/ x 20) " button: " button " key:" modifiers " tags:" tags))
)

(define (mouse-clicked-action x y button cnt modifiers tags)
	(gs:set-text 'MouseDemo 
		(string "clicked row: " (/ y 20) " col:" (/ x 20) 
			" button: " button " count:" cnt " key:" modifiers " tags:" tags))
	(if tags (gs:delete-tag (tags 0)))
)

(define (mouse-moved-action x y)
	(gs:set-text 'MouseDemo 
		(string "moved row: " (/ y 20) " col:" (/ x 20)))
)

(define (mouse-dragged-action x y button modifiers)
	(gs:set-text 'MouseDemo 
		(string "dragged row: " (/ y 20) " col:" (/ x 20) " button: " button " key:" modifiers))
)

(define (mouse-wheel-action x y wheel)
	(gs:set-text 'MouseDemo 
		(string "cursor row: " (/ y 20) " col:" (/ x 20) " wheel: " wheel))
)

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

