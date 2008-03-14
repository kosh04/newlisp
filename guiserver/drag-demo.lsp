#!/usr/bin/newlisp
;;
;; drag-demo.lsp - demonstrate dragging of objects with the mouse
;;

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
;(gs:set-trace true)

;;;; describe the GUI
(gs:frame 'DragDemo  100 100 600 500 "Drag demo")
;(gs:set-resizable 'DragDemo nil)
(gs:canvas 'MyCanvas)
(gs:set-background 'MyCanvas gs:white)
(gs:add-to 'DragDemo 'MyCanvas) 
(gs:mouse-clicked 'MyCanvas 'mouse-clicked-action true)
(gs:mouse-pressed 'MyCanvas 'mouse-pressed-action true)
(gs:mouse-released 'MyCanvas 'mouse-released-action true)
(gs:mouse-dragged 'MyCanvas 'mouse-dragged-action)

;(gs:set-scale 0.5 0.5) ; only for testing, dragging would fail

(gs:set-font 'MyCanvas "Lucida Sans Oblique" 18 "plain")
(gs:draw-text 'T "Drag objects with the mouse, click right for popup menu" 20 250 gs:darkGray)
(gs:fill-circle 'C 100 100 50 gs:red)
(gs:fill-circle 'C 60 60 30 gs:black)
(gs:fill-circle 'C 140 60 30 gs:black)
(gs:fill-circle 'C 100 110 8 gs:yellow)


(gs:draw-polygon 'P '(300 200 400 50 500 200) gs:blue)

(gs:draw-image 'I "/local/newLISP128.png" 300 300 128 128)

(gs:menu-popup 'EditMenuPopup "Edit")
(gs:menu-item 'EditCut 'gs:no-action "Cut")
(gs:menu-item 'EditCopy 'gs:no-action "Copy")
(gs:menu-item 'EditPaste 'gs:no-action "Paste")
(gs:disable 'EditCut 'EditCopy 'EditPaste)
(gs:add-to 'EditMenuPopup 'EditCut 'EditCopy 'EditPaste)

; comment out to test reorder
;(gs:reorder-tags '(I P C T))

(gs:set-visible 'DragDemo true)

;(gs:update)

;; actions
(set 'mouse-down-tags '())

(define (mouse-clicked-action x y button cnt  modifiers tags)
	(gs:set-text 'DragDemo (string  x ":" y " "  button " " cnt " " tags))
	(if (or (= 3 button) (= modifiers 18)) ; check for ctrl-button1-click on Mac
		(gs:show-popup 'EditMenuPopup 'MyCanvas x y))	
)

(define (mouse-pressed-action x y button modifiers tags)
		(gs:set-text 'DragDemo (string  x ":" y " "  button " " modifiers " " tags))
		(set 'mouse-down-tags tags)
		(set 'mouse-old-x x)
		(set 'mouse-old-y y)
)

(define (mouse-released-action x y button modifiers tags)
		(gs:set-text 'DragDemo (string  x ":" y " "  button " " modifiers " " tags))
		(set 'mouse-down-tags '())
)

(define (mouse-dragged-action x y)
	(dolist (t mouse-down-tags)
		(gs:move-tag t (- x mouse-old-x) (- y mouse-old-y)))
	(set 'mouse-old-x x)
	(set 'mouse-old-y y)
)

(gs:listen)

;; eof

