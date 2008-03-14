#!/usr/bin/newlisp
;;
;; allfonts-demo.lsp - show all fonts on the system
;; this program also demonstrates that strings can be used instead 
;; of symbols for widget names (label in this case)

;;;; initialization
(set-locale "C")

(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
;(gs:set-trace true)
;;;; describe the GUI
(gs:frame 'AllFontsDemo 100 100 500 400)
(gs:set-background 'AllFontsDemo 1 1 1)
(gs:get-fonts)
(gs:panel 'FontPanel)
(gs:set-grid-layout 'FontPanel (length gs:fonts) 1 0 0)
(gs:set-text 'AllFontsDemo (string "All " (length gs:fonts) " fonts on this system"))
(dolist (font gs:fonts)
	(set 'font-label (string "label-" $idx))
	(gs:label font-label font)
	(gs:set-size font-label 100 30)
	(gs:set-font font-label font 24 "plain")
	(gs:mouse-event font-label 'mouse-action)
	(gs:add-to 'FontPanel font-label))

(gs:scroll-pane 'Scroll 'FontPanel)
(gs:add-to 'AllFontsDemo 'Scroll)
(gs:set-visible 'AllFontsDemo true)


(define (mouse-action id type x y button cnt mods)
	(gs:set-text 'AllFontsDemo 
		(format "%s %s %d:%d button:%d count:%d mod-key:%d" id type x y button cnt mods))
)
;;;; listen for incoming action requests and dispatch

(gs:listen)

;; eof

