#!/usr/bin/newlisp
;;
;; font-demo.lsp - show all built-in Java fonts (available on all platforms)

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

;;;; describe the GUI
(gs:frame 'TestFrame 100 100 480 320 "newLISP-GS built-in Font Families")
(gs:set-background 'TestFrame 1 1 1)
(gs:set-grid-layout 'TestFrame 12 1)
(gs:label 'L1 "Lucida Sans Regular")
(gs:set-font 'L1 "Lucida Sans" 20 "plain")
(gs:label 'L2 "Lucida Sans Bold")
(gs:set-font 'L2 "Lucida Sans" 20 "bold")
(gs:label 'L3 "Lucida Sans Oblique")
(gs:set-font 'L3 "Lucida Sans" 20 "italic")

(gs:label 'L4 "Lucida Bright Regular")
(gs:set-font 'L4 "Lucida Bright" 20 "plain")
(gs:label 'L5 "Lucida Bright Bold")
(gs:set-font 'L5 "Lucida Bright" 20 "bold")
(gs:label 'L6 "Lucida Bright Oblique")
(gs:set-font 'L6 "Lucida Bright" 20 "italic")

(gs:label 'L7 "Lucida Sans Typewriter Regular")
(gs:set-font 'L7 "Lucida Sans Typewriter" 20 "plain")
(gs:label 'L8 "Lucida Sans Typewriter Bold")
(gs:set-font 'L8 "Lucida Sans Typewriter" 20 "bold")
(gs:label 'L9 "Lucida Sans Typewriter Oblique")
(gs:set-font 'L9 "Lucida Sans Typewriter" 20 "italic")

(gs:label 'L10 "Monospaced Regular")
(gs:set-font 'L10 "Monospaced" 20 "plain")
(gs:label 'L11 "Monospaced Bold")
(gs:set-font 'L11 "Monospaced" 20 "bold")
(gs:label 'L12 "Monospaced Oblique")
(gs:set-font 'L12 "Monospaced" 20 "italic")

(dolist (i '(L1 L2 L3 L4 L5 L6 L7 L8 L9 L10 L11 L12))
	(gs:set-foreground i 0 0 0.4))

(gs:add-to 'TestFrame 'L1 'L2 'L3 'L4 'L5 'L6 'L7 'L8 'L9 'L10 'L11 'L12)
(gs:set-visible 'TestFrame true)

;; actions

(define (toggle-action id flag)
	(if flag
		(gs:set-icon 'aImage "/local/newLISP32.png")
		(gs:set-icon 'aImage "/local/newLISP128.png"))
)

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

