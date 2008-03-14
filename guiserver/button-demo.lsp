#!/usr/bin/newlisp
;;
; button-demo.lsp - demonstrate the button control

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
(gs:set-trace true)

;;;; describe the GUI
(gs:frame 'ButtonDemo 100 100 400 300 "Click on button or color panel")
(gs:set-resizable 'ButtonDemo nil)
(gs:panel 'ColorPanel 360 200)
(gs:set-background 'ColorPanel '(0 1 0) 0.2)
(gs:button 'aButton 'button-action "color")
(gs:set-flow-layout 'ButtonDemo "center" 2 15)
(gs:add-to 'ButtonDemo 'ColorPanel 'aButton)
(gs:set-visible 'ButtonDemo true)

(gs:mouse-event 'ColorPanel 'mouse-action)

;;;; define actions
(define (button-action id)
	(gs:set-color 'ColorPanel (random) (random) (random)))

(define (mouse-action id type x y button cnt mods)
	(gs:set-text 'ButtonDemo (format "%8s  %3d:%3d %d %d %2d" type x  y button cnt mods))
)

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

