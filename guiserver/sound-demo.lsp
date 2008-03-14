#!/usr/bin/newlisp
;;
; button-demo.lsp - demonstrate the button control

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
;(gs:set-trace true)

(constant (global 'home-dir) (or (env "HOME") (env "USERPROFILE") (env "DOCUMENT_ROOT") ""))

;;;; describe the GUI
(gs:frame 'SoundDemo 100 100 200 100 "Sound Demo")
(gs:set-flow-layout 'SoundDemo "center" 20 20)
(gs:button 'aSound 'button-action "Choose Sound File")
(gs:add-to 'SoundDemo 'aSound)
(gs:set-visible 'SoundDemo true)

;;;; define actions
(define (button-action)
	(gs:open-file-dialog 'SoundDemo 'openfile-action  home-dir
		".aif .wav" "Sound files .wav and .aif")
)

(define (openfile-action id  op file)
	(if file (gs:play-sound (base64-dec file)))
)

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

