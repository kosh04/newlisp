#!/usr/bin/newlisp
;;
;; clipboard-demo.lsp - demonstrate the gs:paste-text function

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

(gs:frame 'Frame 200 200 400 300) 
(gs:set-border-layout 'Frame)
(gs:text-area 'TheText 'text-handler)
(gs:button 'TheButton 'button-handler "get clipboard")
(gs:add-to 'Frame 'TheText "center" 'TheButton "south")
(gs:set-visible 'Frame true) 

(define (button-handler)
	(gs:paste-text 'TheText))

(define (text-handler))

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

