#!/usr/bin/newlisp
;;
; textfield-demo.lsp - demonstrate the test-field widget

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 
(gs:set-trace true)

;;;; describe the GUI
(gs:frame 'TextDemo 100 100 400 200 "Enter text with or without password cover")
(gs:label 'aLabel "..." "center" 300 50)
(gs:set-background 'aLabel '(0 1 0) 0.2)
(gs:text-field 'aTextField 'textfield-action 15)

;; as an alternative specify the password character in the text field
;; parameters and leave the check box unchecked, or don't use it at all
;(gs:text-field 'aTextField 'textfield-action 15 "*")
;(gs:check-box 'aCheckBox 'checkbox-action "password" true)

(gs:check-box 'aCheckBox 'checkbox-action "password" nil)
(gs:set-flow-layout 'TextDemo "center" 2 15)
(gs:add-to 'TextDemo 'aLabel 'aTextField 'aCheckBox)
(gs:set-visible 'TextDemo true)

;;;; define actions
(define (textfield-action id text)
	(when text
		(gs:set-text 'aLabel (base64-dec text))))

(define (checkbox-action id flag)
	(if flag 
		(gs:set-echo-char 'aTextField "*")  
		(gs:set-echo-char 'aTextField)) 
) 

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

