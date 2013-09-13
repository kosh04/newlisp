#!/usr/bin/newlisp
;;
;; border-layout-demo.lsp - demonstrate the border layout

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 


;;;; describe the GUI
(gs:frame 'BorderDemo 100 100 400 300 "Border layout demo")
(gs:set-border-layout 'BorderDemo 0 0)
(gs:panel 'NorthPanel 50 50)
(gs:set-color 'NorthPanel 0.5 0 0)
(gs:panel 'WestPanel 50 50)
(gs:set-color 'WestPanel 0.5 0.5 0)
(gs:panel 'CenterPanel 50 50)
(gs:set-color 'CenterPanel 0.0 0.5 0)
(gs:panel 'EastPanel 50 50)
(gs:set-color 'EastPanel 0.0 0.5 0.5)
(gs:panel 'SouthPanel 50 50)
(gs:set-color 'SouthPanel 0.0 0 0.5)
(gs:add-to 'BorderDemo 'NorthPanel "north" 'WestPanel "west" 'CenterPanel "center" 
			'EastPanel "east" 'SouthPanel "south")
(gs:set-visible 'BorderDemo true)

(gs:listen)

;; eof

