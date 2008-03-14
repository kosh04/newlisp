#!/usr/bin/newlisp
;;
;; property-demo.lsp - demonstrate gs:get-screen and gs:get-version

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
;;;; describe the GUI

(gs:get-screen)
(set 'x (/ (- (gs:screen 0) 450) 2))
(set 'y (/ (- (gs:screen 1) 400) 2))

(gs:frame 'PropertyDemo x y 450 400 "Properties")
(gs:text-area 'Output 'gs:no-action')
(gs:set-editable 'Output nil)
(gs:add-to 'PropertyDemo 'Output) 
(gs:set-visible 'PropertyDemo true)

(gs:set-text 'Output (join (map string (gs:get-fonts)) "\n"))
(gs:set-text 'PropertyDemo 
	(string "GUI-server v." (gs:get-version) 
			" screen: " (gs:screen 0) "x" (gs:screen 1)
			", " (gs:screen 2) " dots/inch"))

;;;; listen for incoming action requests and dispatch

(gs:listen)

;; eof

