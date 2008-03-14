#!/usr/bin/newlisp

;; frameless-demo.lsp - frameless and transparent (on MacOS X)

(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

(gs:window 'Window 100 100 400 100)
(gs:set-background 'Window 0 0 0.5 0.2)
(gs:set-border-layout 'Window)
(gs:label 'Time " ") 
(gs:set-font 'Time "Mono Spaced" 40 "bold")
(gs:set-foreground 'Time 1 1 0 )
(gs:add-to 'Window 'Time "center")
(gs:button 'Button 'button-handler "close")
(gs:add-to 'Window 'Button "south")
(gs:set-visible 'Window true)

(define (button-handler) (exit))

(while (gs:check-event 10000)
	(sleep 200) 
	(gs:set-text 'Time (date (date-value) 0 "%b %d %H:%M:%S"))
)

;; eof
