#!/usr/bin/newlisp
;;
; move-resize-demo.lsp - demonstrate gs:window-moved and hs:window-resized events

;;;; initialization
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
;(gs:set-trace true)

;;;; describe the GUI
(gs:frame 'EventDemo 100 100 300 100 "Move or resize window")
(gs:set-border-layout 'EventDemo)
(gs:label 'Position "")
(gs:label 'Size "")
(gs:set-font 'Position "Mono Spaced" 24 "bold")
(gs:set-foreground 'Position 0 0 0.5 )
(gs:set-font 'Size "Mono Spaced" 24 "bold")
(gs:set-foreground 'Size 0 0 0.5 )

(gs:add-to 'EventDemo 'Position "north")
(gs:add-to 'EventDemo 'Size "south")

(gs:window-moved 'EventDemo 'move-action)
(gs:window-resized 'EventDemo 'resize-action)

;;;; define actions

(define (move-action id x y)
	(gs:set-text 'Position (string "Position " x ":" y)))

(define (resize-action id width height)
	(gs:set-text 'Size (string "Size " width ":" height)))


(gs:set-visible 'EventDemo true)

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

