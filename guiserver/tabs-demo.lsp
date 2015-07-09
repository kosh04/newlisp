#!/usr/bin/newlisp
;;
;; tabs-demo.lsp - demonstrate tabs and icons
;; for 10.6.3 ideprecated gs:table-set-row-number was replaced with gs:table-show-row-number

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

(gs:get-screen)
(set 'x (/ (- (gs:screen 0) 450) 2)) 
(set 'y (/ (- (gs:screen 1) 580) 2))

;;;; describe the GUI

(gs:frame 'TheFrame x y 450 580 "Tabbed Pane Demo")
;; When mapping gs:xxx functions the quote before gs:xxx is important
;; so the functions get executed woth gs as the current context which makes
;; the names from wigets be prefixed with their proper context name.
(map 'gs:panel '(first second third icons))
(gs:set-color 'first 1 0 0)
(gs:set-color 'second 0 1 0)
(gs:set-color 'third 0 0 1)
(gs:set-color 'icons 0 0 1 0.1)
(gs:set-flow-layout 'icons "center" 30 50)

(gs:label 'new "new")
(gs:label 'edit "edit")
(gs:label 'cut "cut")
(gs:label 'copy "copy")
(gs:label 'paste "paste")
(gs:label 'folder "folder")
(gs:label 'openfolder "openfolder")
(gs:label 'save "save")
(gs:label 'info "info")
(gs:label 'run "run")
(gs:label 'restart "restart")
(gs:label 'stop "stop")
(gs:label 'fontbook "fontbook")
(gs:label 'clear "clear")
(gs:label 'search "search")

;; if an icon path is prefixed with /local/ it means that this
;; icon is built-in to guiserver.jar. Other paths to user
;; supplied external icon images can be used instead (gif, jpg, png)
(gs:set-icon 'new "/local/new32.png")
(gs:set-icon 'edit "/local/edit32.png")
(gs:set-icon 'cut "/local/cut32.png")
(gs:set-icon 'copy "/local/copy32.png")
(gs:set-icon 'paste "/local/paste32.png")
(gs:set-icon 'folder "/local/folder-closed32.png")
(gs:set-icon 'openfolder "/local/folder-opened32.png")
(gs:set-icon 'save "/local/save32.png")
(gs:set-icon 'info "/local/info32.png")
(gs:set-icon 'run "/local/run32.png")
(gs:set-icon 'restart "/local/restart32.png")
(gs:set-icon 'stop "/local/stop32.png")
(gs:set-icon 'fontbook "/local/font-book32.png")
(gs:set-icon 'clear "/local/clear32.png")
(gs:set-icon 'search "/local/search32.png")

; an alternative way to make labels with icons
(gs:image-label 'newLISP "/local/newLISP32.png")
(gs:set-text 'newLISP "newLISP")
(gs:image-label 'newLISPlogo "/local/newLISP128.png")

(gs:add-to 'icons 'new 'edit 'cut 'copy 'paste 'folder 'openfolder 'save)
(gs:add-to 'icons 'info 'run 'restart 'stop 'fontbook 'clear 'search 'newLISP 'newLISPlogo)

(gs:tabbed-pane 'MyTabs 'tabs-action "top"
			'first "first tab"
			'second "second tab"
			'third "third tab")

(gs:insert-tab 'MyTabs 'icons "icons tab" 3 "/local/newLISP16.png")
;(gs:remove-tab 'MyTabs 0) ; just for test

(gs:table 'Table 'gs:no-action "A" "B" "C")
(gs:table-show-row-number 'Table true)
(gs:table-add-row 'Table "1 A" "1 B" "1 C")
(gs:table-add-row 'Table "2 A" "2 B" "2 C")
(gs:table-add-row 'Table "3 A" "3 B" "3 C")
(gs:table-add-row 'Table "4 A" "4 B" "4 C")
(gs:insert-tab 'MyTabs 'Table "Table tab" 4)

(gs:request-focus 'MyTabs 3)
(gs:add-to 'TheFrame 'MyTabs) 
(gs:set-visible 'TheFrame true)

;;;; define actions

(define (tabs-action id cid tab)
	(println id " " cid " " (base64-dec tab)))

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

