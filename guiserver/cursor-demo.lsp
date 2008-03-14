#!/usr/bin/newlisp
;;
;; cursor-demo - demonstrate cursor shapes with gs:set-cursor

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
;(gs:set-trace true)

(set 'cursor-shapes '(
		"default"
		"crosshair"
		"text"
		"wait"
		"sw-resize"
		"se-resize"
		"nw-resize"
		"ne-resize"
		"n-resize"
		"s-resize"
		"w-resize"
		"e-resize"
		"hand"
		"move"
))

;; describe the GUI
(gs:frame 'CursorDemo 100 100 500 200 "Cursor shapes - move the cursor over the panels")
(gs:set-grid-layout 'CursorDemo 2 7)

(dolist (c cursor-shapes)
	(set 'id (sym c))
	(gs:panel id)
	(gs:set-background id (list (random) (random) (random)))
	(gs:set-cursor id c)
	(gs:add-to 'CursorDemo id))

;(gs:set-resizable 'CursorDemo nil)

(gs:set-visible 'CursorDemo true)

(gs:listen)

;; eof

